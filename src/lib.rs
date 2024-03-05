use std::fmt;

#[derive(Debug, PartialEq)]
pub enum ParseError {
  UnmatchedCloser(String),
  MismatchedCloser(String, String),
  UnclosedOpener(String),
  UnclosedString,
}

fn is_whitespace(c: char) -> bool {
  c == ' ' || c == ',' || c == '\t' || c == '\n'
}

#[derive(Clone, PartialEq, Debug)]
pub enum Sexp {
  List(Vec<Sexp>),
  Leaf(String),
}

impl fmt::Display for Sexp {
  fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
    match &self {
      Sexp::Leaf(token) => fmt.write_str(token)?,
      Sexp::List(sub_expressions) => {
        fmt.write_str("(")?;
        let mut separator = "";
        for sexp in sub_expressions {
          fmt.write_str(separator)?;
          fmt.write_str(&sexp.to_string())?;
          separator = " ";
        }
        fmt.write_str(")")?;
      }
    }
    Ok(())
  }
}

pub trait Delimiter {
  fn opener(&self) -> String;
  fn closer(&self) -> String;
  fn tag(&self) -> Option<String>;
}

pub trait Prefix {
  fn marker(&self) -> String;
  fn tag(&self) -> String;
}

pub enum GSexp<DelimiterType: Delimiter, PrefixType: Prefix> {
  Prefixed(PrefixType, Box<GSexp<DelimiterType, PrefixType>>),
  Delimited(DelimiterType, Vec<GSexp<DelimiterType, PrefixType>>),
  Leaf(String),
}

impl<DelimiterType: Delimiter, PrefixType: Prefix>
  GSexp<DelimiterType, PrefixType>
{
  pub fn to_sexp(&self) -> Sexp {
    match self {
      GSexp::Leaf(token) => Sexp::Leaf(token.clone()),
      GSexp::Prefixed(prefix, sub_expression) => {
        Sexp::List(vec![Sexp::Leaf(prefix.tag()), sub_expression.to_sexp()])
      }
      GSexp::Delimited(delimiter, sub_expressions) => match delimiter.tag() {
        Some(tag) => Sexp::List(
          std::iter::once(Sexp::Leaf(tag))
            .chain(sub_expressions.iter().map(|e| e.to_sexp()))
            .collect(),
        ),
        None => {
          Sexp::List(sub_expressions.iter().map(|e| e.to_sexp()).collect())
        }
      },
    }
  }
}

pub fn chars_match(pattern: &[char], chars: &[char]) -> bool {
  chars.len() >= pattern.len()
    && match (0..pattern.len())
      .map(|i| pattern[i] == chars[i])
      .reduce(|acc, e| acc && e)
    {
      None => true,
      Some(b) => b,
    }
}

enum PartialGSexp<DelimiterType: Delimiter, PrefixType: Prefix> {
  Prefixed(PrefixType),
  Delimited(DelimiterType, Vec<GSexp<DelimiterType, PrefixType>>),
}
struct ParserState<DelimiterType: Delimiter, PrefixType: Prefix> {
  expressions: Vec<GSexp<DelimiterType, PrefixType>>,
  partial_expression: Vec<PartialGSexp<DelimiterType, PrefixType>>,
}
impl<DelimiterType: Delimiter, PrefixType: Prefix>
  ParserState<DelimiterType, PrefixType>
{
  pub fn new() -> ParserState<DelimiterType, PrefixType> {
    ParserState {
      expressions: vec![],
      partial_expression: vec![],
    }
  }
  pub fn insert_gsexp(&mut self, mut gsexp: GSexp<DelimiterType, PrefixType>) {
    loop {
      if let Some(next_expressions) = self.partial_expression.pop() {
        match next_expressions {
          PartialGSexp::Prefixed(prefix_type) => {
            gsexp = GSexp::Prefixed(prefix_type, Box::new(gsexp))
          }
          PartialGSexp::Delimited(delimiter_type, mut sub_expressions) => {
            sub_expressions.push(gsexp);
            self
              .partial_expression
              .push(PartialGSexp::Delimited(delimiter_type, sub_expressions));
            break;
          }
        }
      } else {
        self.expressions.push(gsexp);
        break;
      }
    }
  }
  pub fn insert_leaf(&mut self, token: String) {
    self.insert_gsexp(GSexp::Leaf(token));
  }
  pub fn open_prefix(&mut self, prefix_type: PrefixType) {
    self
      .partial_expression
      .push(PartialGSexp::Prefixed(prefix_type));
  }
  pub fn open_delimiter(&mut self, delimiter_type: DelimiterType) {
    self
      .partial_expression
      .push(PartialGSexp::Delimited(delimiter_type, vec![]));
  }
  pub fn close_delimiter(&mut self) {
    match self.partial_expression.pop() {
      Some(PartialGSexp::Delimited(delimiter_type, sub_expressions)) => {
        self.insert_gsexp(GSexp::Delimited(delimiter_type, sub_expressions))
      }
      Some(PartialGSexp::Prefixed(_)) => panic!(
        "tried to close delimiter with an open prefix on the top of \
        partial_expression stack"
      ),
      None => {
        panic!(
          "tried to close delimiter with nothing on partial_expression stack"
        )
      }
    }
  }
}

pub fn parse_chars<DelimiterType: Delimiter, PrefixType: Prefix>(
  delimiters: &[DelimiterType],
  prefixes: &[PrefixType],
  chars: Vec<char>,
) -> Result<Sexp, ParseError> {
  /*if chars.is_empty() {
    return Ok(Sexp::Leaf("".to_string()));
  }

  let parser_state: &mut ParserState = &mut ParserState::new();
  let mut expression_opened: bool = false;
  let mut char_index: usize = 0;
  let mut consumed_index: usize = 0;

  loop {
    if char_index >= chars.len() {
      break;
    }
    let char = chars[char_index];
    let string_opener = char == '"';

    let prefix_index = if consumed_index == char_index {
      (0..prefixes.len()).rev().find(|prefix_index| {
        let marker: Vec<char> =
          prefixes[*prefix_index].marker().chars().collect();
        chars_match(&marker, &chars[char_index..])
      })
    } else {
      None
    };

    let matched_closing_delimiter = delimiters.iter().find(|delimiter| {
      let closer: Vec<char> = delimiter.closer().chars().collect();
      chars_match(&closer, &chars[char_index..])
    });
    let open_list = parser_state.get_open_list();
    let was_expected_closer_matched = match &open_list {
      Some((_, closer)) => chars_match(&closer, &chars[char_index..]),
      None => false,
    };
    match &matched_closing_delimiter {
      None => (),
      Some(delimiter) => match open_list {
        None => return Err(ParseError::UnmatchedCloser(delimiter.closer())),
        Some((opener, _)) => {
          if !was_expected_closer_matched {
            return Err(ParseError::MismatchedCloser(
              opener.iter().collect(),
              delimiter.closer(),
            ));
          }
        }
      },
    }

    let matched_opening_delimiter = delimiters.iter().rev().find(|delimiter| {
      let opener: Vec<char> = delimiter.opener().chars().collect();
      chars_match(&opener, &chars[char_index..])
    });
    if !was_expected_closer_matched && matched_closing_delimiter.is_some() {}
    let whitespace = prefix_index.is_none()
      && matched_opening_delimiter.is_none()
      && !was_expected_closer_matched
      && is_whitespace(char);
    if consumed_index != char_index
      && (string_opener
        || matched_opening_delimiter.is_some()
        || was_expected_closer_matched
        || whitespace)
    {
      // If consumption isn't up to the current index, and this character
      // indicates the previous token should end, add previous token to AST.
      parser_state
        .insert_token(chars[consumed_index..char_index].iter().collect());
      if whitespace {
        // If this character is whitespace, indicating that this was the end of
        // a terminal token rather than a closing delimiter, close any open
        // prefixes.

        parser_state.close_prefixes();
      }
      if !expression_opened {
        return Ok(parser_state.finish());
      }
    };
    if string_opener {
      // If this character is a ", indicating the start of a string, consume
      // the entire string.
      let string_start_index = char_index;
      loop {
        char_index += 1;
        if char_index >= chars.len() {
          return Err(ParseError::UnclosedString);
        }
        let string_char = chars[char_index];
        if string_char == '\\' {
          char_index += 1
        } else if string_char == '"' {
          parser_state.insert_token(
            chars[string_start_index..char_index + 1].iter().collect(),
          );
          break;
        }
      }
      if !expression_opened {
        return Ok(parser_state.finish());
      }
    }
    // Handle the case where this character is the start of a prefix, opener,
    // or closer, and adjust the character index accordingly.
    char_index += match prefix_index {
      Some(i) => {
        expression_opened = true;
        // If this is a prefix, add a list, with the tag of the prefix as the
        // first element of the list, to the AST.
        let prefix_chars: Vec<char> = prefixes[i].marker().chars().collect();
        let tag = prefixes[i].tag();
        parser_state.open_prefix(char_index, &tag);
        prefix_chars.len()
      }
      None => match matched_opening_delimiter {
        Some(delimiter) => {
          let opener: Vec<char> = delimiter.opener().chars().collect();
          let closer: Vec<char> = delimiter.closer().chars().collect();
          let tag = delimiter.tag();
          expression_opened = true;
          // If this is an opener, add a list, with the tag of the delimiter
          // pair (if it has one) as the first element of the list, to the AST.
          parser_state.open_list(
            char_index,
            opener.clone(),
            closer.clone(),
            &tag,
          );
          opener.len()
        }
        None => {
          if was_expected_closer_matched {
            // This unwrap is safe, because expected_closer_matched will only
            // be true if there is a list to close.
            parser_state.close_list().unwrap().len()
          } else {
            1
          }
        }
      },
    };
    // Catch the consumption up to the current position, if the current
    // character matched with something for which that should be done.
    if string_opener
      || prefix_index.is_some()
      || matched_opening_delimiter.is_some()
      || was_expected_closer_matched
      || whitespace
    {
      consumed_index = char_index;
    }
    if expression_opened && parser_state.opening_stack.len() == 0 {
      return Ok(parser_state.finish());
    }
  }

  // If the end of the string was reached and consumption isn't caught up, the
  // string must end with a token not followed by whitespace, so add one
  // final leaf to the AST.
  if char_index >= chars.len() && consumed_index < char_index {
    parser_state
      .insert_token(chars[consumed_index..chars.len()].iter().collect());
  }
  parser_state.close_prefixes();

  // Throw an error if there are any open lists at the end of the string.
  match parser_state.get_open_list() {
    Some((opener, _)) => {
      return Err(ParseError::UnclosedOpener(opener.iter().collect()))
    }
    None => (),
  }
  return Ok(parser_state.finish());*/
  todo!()
}

pub fn parse<DelimiterType: Delimiter, PrefixType: Prefix>(
  delimiters: &[DelimiterType],
  prefixes: &[PrefixType],
  s: &str,
) -> Result<Sexp, ParseError> {
  parse_chars(delimiters, prefixes, s.chars().collect())
}

#[allow(dead_code)]
mod tests {
  use super::*;

  enum TestDelimiter {
    Parentheses,
    Brackets,
    Braces,
    HashBraces,
    HashBrackets,
  }

  impl Delimiter for TestDelimiter {
    fn opener(&self) -> String {
      match self {
        TestDelimiter::Parentheses => "(".to_string(),
        TestDelimiter::Brackets => "[".to_string(),
        TestDelimiter::Braces => "{".to_string(),
        TestDelimiter::HashBrackets => "#[".to_string(),
        TestDelimiter::HashBraces => "#{".to_string(),
      }
    }

    fn closer(&self) -> String {
      match self {
        TestDelimiter::Parentheses => ")".to_string(),
        TestDelimiter::Brackets => "]".to_string(),
        TestDelimiter::Braces => "}".to_string(),
        TestDelimiter::HashBrackets => "]".to_string(),
        TestDelimiter::HashBraces => "}".to_string(),
      }
    }

    fn tag(&self) -> Option<String> {
      match self {
        TestDelimiter::Parentheses => None,
        TestDelimiter::Brackets => Some("#brackets".to_string()),
        TestDelimiter::Braces => Some("#braces".to_string()),
        TestDelimiter::HashBrackets => Some("#hash-brackets".to_string()),
        TestDelimiter::HashBraces => Some("#hash-braces".to_string()),
      }
    }
  }

  const TEST_DELIMITERS: [TestDelimiter; 5] = [
    TestDelimiter::Parentheses,
    TestDelimiter::Brackets,
    TestDelimiter::Braces,
    TestDelimiter::HashBrackets,
    TestDelimiter::HashBraces,
  ];

  enum TestPrefix {
    Quote,
    Unquote,
  }

  impl Prefix for TestPrefix {
    fn marker(&self) -> String {
      match self {
        TestPrefix::Quote => "'".to_string(),
        TestPrefix::Unquote => "~".to_string(),
      }
    }

    fn tag(&self) -> String {
      match self {
        TestPrefix::Quote => "quote".to_string(),
        TestPrefix::Unquote => "unquote".to_string(),
      }
    }
  }

  const TEST_PREFIXES: [TestPrefix; 2] =
    [TestPrefix::Quote, TestPrefix::Unquote];

  type TestGSexp = GSexp<TestDelimiter, TestPrefix>;

  #[test]
  fn test_gsexp_to_sexp() {
    assert_eq!(
      TestGSexp::Leaf("hello".to_owned()).to_sexp(),
      Sexp::Leaf("hello".to_owned())
    );
    assert_eq!(
      TestGSexp::Prefixed(
        TestPrefix::Quote,
        Box::new(TestGSexp::Leaf("hello".to_owned()))
      )
      .to_sexp(),
      Sexp::List(vec![
        Sexp::Leaf("quote".to_owned()),
        Sexp::Leaf("hello".to_owned())
      ])
    );
    assert_eq!(
      TestGSexp::Delimited(
        TestDelimiter::Brackets,
        vec![TestGSexp::Leaf("hello".to_owned())]
      )
      .to_sexp(),
      Sexp::List(vec![
        Sexp::Leaf("#brackets".to_owned()),
        Sexp::Leaf("hello".to_owned())
      ])
    );
  }

  macro_rules! assert_parse_eq {
    ($string:literal, $sexp:expr) => {
      let parsed_sexp = parse(&TEST_DELIMITERS, &TEST_PREFIXES, $string)
        .expect("Failted to parse sexp");
      assert_eq!(
        $sexp, parsed_sexp,
        "String {:?} was not parsed as expected.",
        $string
      );
    };
  }

  #[test]
  fn test_parse_0() {
    assert_parse_eq!("", Sexp::Leaf("".to_string()));
  }

  #[test]
  fn test_parse_1() {
    assert_parse_eq!("()", Sexp::List(vec![]));
  }

  #[test]
  fn test_parse_2() {
    assert_parse_eq!(
      "[]",
      Sexp::List(vec![Sexp::Leaf("#brackets".to_string())])
    );
  }

  #[test]
  fn test_parse_3() {
    assert_parse_eq!("{}", Sexp::List(vec![Sexp::Leaf("#braces".to_string())]));
  }

  #[test]
  fn test_parse_4() {
    assert_parse_eq!("hello!", Sexp::Leaf("hello!".to_string()));
  }

  #[test]
  fn test_parse_5() {
    assert_parse_eq!(
      "(+ 1 2)",
      Sexp::List(vec![
        Sexp::Leaf("+".to_string()),
        Sexp::Leaf("1".to_string()),
        Sexp::Leaf("2".to_string()),
      ])
    );
  }

  #[test]
  fn test_parse_6() {
    assert_parse_eq!(
      "'(+ 1 2)",
      Sexp::List(vec![
        Sexp::Leaf("quote".to_string()),
        Sexp::List(vec![
          Sexp::Leaf("+".to_string()),
          Sexp::Leaf("1".to_string()),
          Sexp::Leaf("2".to_string()),
        ]),
      ])
    );
  }

  #[test]
  fn test_parse_7() {
    assert_parse_eq!(
      "~'()",
      Sexp::List(vec![
        Sexp::Leaf("unquote".to_string()),
        Sexp::List(vec![Sexp::Leaf("quote".to_string()), Sexp::List(vec![])]),
      ])
    );
  }

  #[test]
  fn test_parse_8() {
    assert_parse_eq!(
      "'a",
      Sexp::List(vec![
        Sexp::Leaf("quote".to_string()),
        Sexp::Leaf("a".to_string()),
      ])
    );
  }

  #[test]
  fn test_parse_9() {
    assert_parse_eq!(
      "''a",
      Sexp::List(vec![
        Sexp::Leaf("quote".to_string()),
        Sexp::List(vec![
          Sexp::Leaf("quote".to_string()),
          Sexp::Leaf("a".to_string()),
        ]),
      ])
    );
  }
}
