use std::fmt;
use std::fmt::Debug;

#[derive(Debug, PartialEq)]
pub enum ParseError {
  UnmatchedCloser(String),
  MismatchedCloser(String, String),
  UnclosedOpener(String),
  TrailingPrefix(String),
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

pub trait Delimiter: Clone + Debug {
  fn opener(&self) -> String;
  fn closer(&self) -> String;
  fn tag(&self) -> Option<String>;
  fn is_symmetric(&self) -> bool {
    self.opener() == self.closer()
  }
}

pub trait Prefix: Clone + Debug {
  fn marker(&self) -> String;
  fn tag(&self) -> String;
}

#[derive(Debug)]
pub enum GSexp<DelimiterType: Delimiter, PrefixType: Prefix> {
  Prefixed(PrefixType, Box<GSexp<DelimiterType, PrefixType>>),
  Delimited(DelimiterType, Vec<GSexp<DelimiterType, PrefixType>>),
  Leaf(String),
}

fn is_prefix(prefix: &[char], chars: &[char]) -> bool {
  chars.len() >= prefix.len()
    && match (0..prefix.len())
      .map(|i| prefix[i] == chars[i])
      .reduce(|acc, e| acc && e)
    {
      None => true,
      Some(b) => b,
    }
}

fn is_string_prefix(prefix_str: String, chars: &[char]) -> bool {
  let prefix_chars: Vec<char> = prefix_str.chars().collect();
  is_prefix(&prefix_chars, chars)
}

#[derive(Debug)]
enum PartialGSexp<DelimiterType: Delimiter, PrefixType: Prefix> {
  Prefixed(PrefixType),
  Delimited(DelimiterType, Vec<GSexp<DelimiterType, PrefixType>>),
}
#[derive(Debug)]
struct ParserState<DelimiterType: Delimiter, PrefixType: Prefix> {
  expressions: Vec<GSexp<DelimiterType, PrefixType>>,
  partial_expression: Vec<PartialGSexp<DelimiterType, PrefixType>>,
}
impl<DelimiterType: Delimiter, PrefixType: Prefix>
  ParserState<DelimiterType, PrefixType>
{
  fn new() -> ParserState<DelimiterType, PrefixType> {
    ParserState {
      expressions: vec![],
      partial_expression: vec![],
    }
  }
  fn insert_gsexp(&mut self, mut gsexp: GSexp<DelimiterType, PrefixType>) {
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
  fn insert_leaf(&mut self, token: String) {
    self.insert_gsexp(GSexp::Leaf(token));
  }
  fn open_prefix(&mut self, prefix_type: PrefixType) {
    self
      .partial_expression
      .push(PartialGSexp::Prefixed(prefix_type));
  }
  fn open_delimiter(&mut self, delimiter_type: DelimiterType) {
    self
      .partial_expression
      .push(PartialGSexp::Delimited(delimiter_type, vec![]));
  }
  fn close_delimiter(&mut self) {
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
  fn get_open_delimiter(&self) -> Option<DelimiterType> {
    self
      .partial_expression
      .iter()
      .rev()
      .find_map(|partial| match partial {
        PartialGSexp::Prefixed(_) => None,
        PartialGSexp::Delimited(delimiter_type, _) => {
          Some(delimiter_type.clone())
        }
      })
  }
  fn get_open_partial(
    &self,
  ) -> Option<&PartialGSexp<DelimiterType, PrefixType>> {
    self.partial_expression.last()
  }
  fn take_last_completed_expression(
    &mut self,
  ) -> Option<GSexp<DelimiterType, PrefixType>> {
    self.expressions.pop()
  }
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
  pub fn parse(
    delimiters: &[DelimiterType],
    prefixes: &[PrefixType],
    string: &str,
  ) -> Result<Self, ParseError> {
    let chars: Vec<char> = string.chars().collect();
    if string.is_empty() {
      return Ok(GSexp::Leaf("".to_string()));
    }

    let mut delimiters_by_opener_size: Vec<DelimiterType> =
      delimiters.iter().cloned().collect();
    delimiters_by_opener_size
      .sort_by_cached_key(|delimiter| -(delimiter.opener().len() as i32));

    let mut delimiters_by_closer_size: Vec<DelimiterType> =
      delimiters.iter().cloned().collect();
    delimiters_by_closer_size
      .sort_by_cached_key(|delimiter| -(delimiter.closer().len() as i32));

    let mut prefixes_by_size: Vec<PrefixType> =
      prefixes.iter().cloned().collect();
    prefixes_by_size
      .sort_by_cached_key(|prefix| -(prefix.marker().len() as i32));

    let parser_state: &mut ParserState<DelimiterType, PrefixType> =
      &mut ParserState::new();
    let mut char_index: usize = 0;
    let mut consumed_index: usize = 0;

    loop {
      if char_index >= string.len() {
        break;
      }
      let char = chars[char_index];
      let string_opener = char == '"';

      let matched_prefix = if consumed_index == char_index {
        prefixes_by_size.iter().find(|prefix| {
          is_string_prefix(prefix.marker(), &chars[char_index..])
        })
      } else {
        None
      };

      let matched_closing_delimiter =
        delimiters_by_closer_size.iter().find(|delimiter| {
          is_string_prefix(delimiter.closer(), &chars[char_index..])
        });
      let maybe_open_delimiter = parser_state.get_open_delimiter();
      let was_expected_closer_matched = match &maybe_open_delimiter {
        Some(open_delimiter) => {
          is_string_prefix(open_delimiter.closer(), &chars[char_index..])
        }
        None => false,
      };
      match &matched_closing_delimiter {
        None => (),
        Some(closing_delimiter) => match &maybe_open_delimiter {
          None => {
            if !closing_delimiter.is_symmetric() {
              return Err(ParseError::UnmatchedCloser(
                closing_delimiter.closer(),
              ));
            }
          }
          Some(opening_delimiter) => {
            if !was_expected_closer_matched && !closing_delimiter.is_symmetric()
            {
              return Err(ParseError::MismatchedCloser(
                opening_delimiter.opener(),
                closing_delimiter.closer(),
              ));
            }
          }
        },
      }

      let matched_opening_delimiter =
        delimiters_by_opener_size.iter().find(|delimiter| {
          is_string_prefix(delimiter.opener(), &chars[char_index..])
        });
      if !was_expected_closer_matched
        && matched_closing_delimiter.is_some()
        && !matched_closing_delimiter.unwrap().is_symmetric()
      {
        return Err(ParseError::MismatchedCloser(
          maybe_open_delimiter.unwrap().opener(),
          matched_closing_delimiter.unwrap().closer(),
        ));
      }
      let whitespace = matched_prefix.is_none()
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
          .insert_leaf(chars[consumed_index..char_index].iter().collect());
        if let Some(completed_expression) =
          parser_state.take_last_completed_expression()
        {
          return Ok(completed_expression);
        }
      };
      if string_opener {
        // If this character is a ", indicating the start of a string, consume
        // the entire string.
        let string_start_index = char_index;
        loop {
          char_index += 1;
          if char_index >= string.len() {
            return Err(ParseError::UnclosedString);
          }
          let string_char = chars[char_index];
          if string_char == '\\' {
            char_index += 1
          } else if string_char == '"' {
            parser_state.insert_leaf(
              chars[string_start_index..char_index + 1].iter().collect(),
            );
            break;
          }
        }
        if let Some(completed_expression) =
          parser_state.take_last_completed_expression()
        {
          return Ok(completed_expression);
        }
      }
      // Handle the case where this character is the start of a prefix, opener,
      // or closer, and adjust the character index accordingly.
      char_index += match matched_prefix {
        Some(prefix) => {
          // If this is a prefix, add a list, with the tag of the prefix as the
          // first element of the list, to the AST.
          parser_state.open_prefix(prefix.clone());
          prefix.marker().len()
        }
        None => {
          if was_expected_closer_matched {
            // This unwrap is safe, because was_expected_closer_matched will
            // only be true if there is a list to close.
            parser_state.close_delimiter();
            maybe_open_delimiter.unwrap().closer().len()
          } else {
            match matched_opening_delimiter {
              Some(delimiter) => {
                // If this is an opener, open a delimiter.
                parser_state.open_delimiter(delimiter.clone());
                delimiter.opener().len()
              }
              None => 1,
            }
          }
        }
      };
      // Catch the consumption up to the current position, if the current
      // character matched with something for which that should be done.
      if string_opener
        || matched_prefix.is_some()
        || matched_opening_delimiter.is_some()
        || was_expected_closer_matched
        || whitespace
      {
        consumed_index = char_index;
      }
      if let Some(completed_expression) =
        parser_state.take_last_completed_expression()
      {
        return Ok(completed_expression);
      }
      //println!("{:?}", parser_state);
    }

    // If the end of the string was reached and consumption isn't caught up, the
    // string must end with a token not followed by whitespace, so add one
    // final leaf to the AST.
    if char_index >= string.len() && consumed_index < char_index {
      parser_state
        .insert_leaf(chars[consumed_index..string.len()].iter().collect());
    }

    // Throw an error if there are any open lists at the end of the string.
    if let Some(open_partial) = parser_state.get_open_partial() {
      return Err(match open_partial {
        PartialGSexp::Prefixed(prefix) => {
          ParseError::TrailingPrefix(prefix.marker())
        }
        PartialGSexp::Delimited(delimiter, _) => {
          ParseError::UnclosedOpener(delimiter.opener())
        }
      });
    }
    Ok(parser_state.take_last_completed_expression().unwrap())
  }
}

#[derive(Clone, Debug)]
pub struct SimpleDelimiter {
  opener: String,
  closer: String,
  tag: Option<String>,
}
impl Delimiter for SimpleDelimiter {
  fn opener(&self) -> String {
    self.opener.clone()
  }

  fn closer(&self) -> String {
    self.closer.clone()
  }

  fn tag(&self) -> Option<String> {
    self.tag.clone()
  }
}
#[derive(Clone, Debug)]
pub struct SimplePrefix {
  marker: String,
  tag: String,
}
impl Prefix for SimplePrefix {
  fn marker(&self) -> String {
    self.marker.clone()
  }

  fn tag(&self) -> String {
    self.tag.clone()
  }
}
pub fn generate_simple_delimiters_and_prefixes(
  delimiter_strings: Vec<(&str, &str, Option<&str>)>,
  prefix_strings: Vec<(&str, &str)>,
) -> (Vec<SimpleDelimiter>, Vec<SimplePrefix>) {
  (
    delimiter_strings
      .into_iter()
      .map(|(opener, closer, tag)| SimpleDelimiter {
        opener: opener.to_string(),
        closer: closer.to_string(),
        tag: tag.map(|s| s.to_string()),
      })
      .collect(),
    prefix_strings
      .into_iter()
      .map(|(marker, tag)| SimplePrefix {
        marker: marker.to_string(),
        tag: tag.to_string(),
      })
      .collect(),
  )
}

#[allow(dead_code, unused_macros)]
mod tests {
  use super::*;

  #[derive(Clone, Debug)]
  enum TestDelimiter {
    Parentheses,
    Brackets,
    Braces,
    HashBraces,
    HashBrackets,
    Pipe,
  }

  impl Delimiter for TestDelimiter {
    fn opener(&self) -> String {
      match self {
        TestDelimiter::Parentheses => "(".to_string(),
        TestDelimiter::Brackets => "[".to_string(),
        TestDelimiter::Braces => "{".to_string(),
        TestDelimiter::HashBrackets => "#[".to_string(),
        TestDelimiter::HashBraces => "#{".to_string(),
        TestDelimiter::Pipe => "|".to_string(),
      }
    }

    fn closer(&self) -> String {
      match self {
        TestDelimiter::Parentheses => ")".to_string(),
        TestDelimiter::Brackets => "]".to_string(),
        TestDelimiter::Braces => "}".to_string(),
        TestDelimiter::HashBrackets => "]".to_string(),
        TestDelimiter::HashBraces => "}".to_string(),
        TestDelimiter::Pipe => "|".to_string(),
      }
    }

    fn tag(&self) -> Option<String> {
      match self {
        TestDelimiter::Parentheses => None,
        TestDelimiter::Brackets => Some("#brackets".to_string()),
        TestDelimiter::Braces => Some("#braces".to_string()),
        TestDelimiter::HashBrackets => Some("#hash-brackets".to_string()),
        TestDelimiter::HashBraces => Some("#hash-braces".to_string()),
        TestDelimiter::Pipe => Some("#pipe".to_string()),
      }
    }
  }

  const TEST_DELIMITERS: [TestDelimiter; 6] = [
    TestDelimiter::Parentheses,
    TestDelimiter::Brackets,
    TestDelimiter::Braces,
    TestDelimiter::HashBrackets,
    TestDelimiter::HashBraces,
    TestDelimiter::Pipe,
  ];

  #[derive(Clone, Debug)]
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

  macro_rules! assert_parse_eq {
    ($string:literal, $sexp:expr) => {
      let parsed_sexp =
        TestGSexp::parse(&TEST_DELIMITERS, &TEST_PREFIXES, $string)
          .expect("Failted to parse string")
          .to_sexp();
      assert_eq!(
        $sexp, parsed_sexp,
        "String {:?} was not parsed as expected.",
        $string
      );
    };
  }

  macro_rules! assert_parameterized_parse_eq {
    ($delimiters:expr, $prefixes:expr, $string:literal, $sexp:expr) => {
      let (delimiters, prefixes) =
        generate_simple_delimiters_and_prefixes($delimiters, $prefixes);
      let gsexp: GSexp<SimpleDelimiter, SimplePrefix> =
        GSexp::parse(&delimiters, &prefixes, $string)
          .expect("Failed to parse string");
      assert_eq!(gsexp.to_sexp(), $sexp);
    };
  }

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

  #[test]
  fn test_parse_empty() {
    assert_parse_eq!("", Sexp::Leaf("".to_string()));
  }

  #[test]
  fn test_parse_empty_list() {
    assert_parse_eq!("()", Sexp::List(vec![]));
  }

  #[test]
  fn test_parse_empty_brackets() {
    assert_parse_eq!(
      "[]",
      Sexp::List(vec![Sexp::Leaf("#brackets".to_string())])
    );
  }

  #[test]
  fn test_parse_nested_delimiters() {
    assert_parse_eq!(
      "{[]}",
      Sexp::List(vec![
        Sexp::Leaf("#braces".to_string()),
        Sexp::List(vec![Sexp::Leaf("#brackets".to_string())])
      ])
    );
  }

  #[test]
  fn test_parse_solo_token() {
    assert_parse_eq!("hello!", Sexp::Leaf("hello!".to_string()));
  }

  #[test]
  fn test_parse_list() {
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
  fn test_parse_prefixed_nested_delimiter() {
    assert_parse_eq!(
      "'(+ 1 2 (* 3 4))",
      Sexp::List(vec![
        Sexp::Leaf("quote".to_string()),
        Sexp::List(vec![
          Sexp::Leaf("+".to_string()),
          Sexp::Leaf("1".to_string()),
          Sexp::Leaf("2".to_string()),
          Sexp::List(vec![
            Sexp::Leaf("*".to_string()),
            Sexp::Leaf("3".to_string()),
            Sexp::Leaf("4".to_string()),
          ]),
        ]),
      ])
    );
  }

  #[test]
  fn test_parse_prefix() {
    assert_parse_eq!(
      "'a",
      Sexp::List(vec![
        Sexp::Leaf("quote".to_string()),
        Sexp::Leaf("a".to_string()),
      ])
    );
  }

  #[test]
  fn test_parse_prefixed_inside_delimiter() {
    assert_parse_eq!(
      "('a)",
      Sexp::List(vec![Sexp::List(vec![
        Sexp::Leaf("quote".to_string()),
        Sexp::Leaf("a".to_string()),
      ])])
    );
  }

  #[test]
  fn test_parse_prefixed_and_not_prefixed_inside_delimiter() {
    assert_parse_eq!(
      "('a b)",
      Sexp::List(vec![
        Sexp::List(vec![
          Sexp::Leaf("quote".to_string()),
          Sexp::Leaf("a".to_string()),
        ]),
        Sexp::Leaf("b".to_string())
      ])
    );
  }

  #[test]
  fn test_parse_nested_prefixes() {
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

  #[test]
  fn test_parse_prefixes_and_delimiters() {
    assert_parse_eq!(
      "~'()",
      Sexp::List(vec![
        Sexp::Leaf("unquote".to_string()),
        Sexp::List(vec![Sexp::Leaf("quote".to_string()), Sexp::List(vec![])]),
      ])
    );
  }

  #[test]
  fn test_parse_empty_string() {
    assert_parse_eq!("\"\"", Sexp::Leaf("\"\"".to_string()));
  }

  #[test]
  fn test_parse_string() {
    assert_parse_eq!(
      "\"hello world!\"",
      Sexp::Leaf("\"hello world!\"".to_string())
    );
  }

  #[test]
  fn test_parse_string_inside_delimiter() {
    assert_parse_eq!(
      "(\"hello world!\" \"goodbye world!\")",
      Sexp::List(vec![
        Sexp::Leaf("\"hello world!\"".to_string()),
        Sexp::Leaf("\"goodbye world!\"".to_string()),
      ])
    );
  }

  #[test]
  fn test_parse_delimiter_characters_inside_string() {
    assert_parse_eq!(
      "(\"])]]]]}}(\")",
      Sexp::List(vec![Sexp::Leaf("\"])]]]]}}(\"".to_string()),])
    );
  }

  #[test]
  fn test_ambiguous_delimiters() {
    assert_parameterized_parse_eq!(
      vec![("(", ")", None), ("((", "))", Some("#double-parens"))],
      vec![],
      "(())",
      Sexp::List(vec![Sexp::Leaf("#double-parens".to_string())])
    );
  }

  #[test]
  fn test_nested_ambiguous_delimiters() {
    assert_parameterized_parse_eq!(
      vec![("(", ")", None), ("((", "))", Some("#double-parens"))],
      vec![],
      "((()))",
      Sexp::List(vec![
        Sexp::Leaf("#double-parens".to_string()),
        Sexp::List(vec![])
      ])
    );
  }

  #[test]
  fn test_ambiguous_prefixes() {
    assert_parameterized_parse_eq!(
      vec![],
      vec![("'", "#quote"), ("''", "#double-quote")],
      "''a",
      Sexp::List(vec![
        Sexp::Leaf("#double-quote".to_string()),
        Sexp::Leaf("a".to_string())
      ])
    );
  }

  #[test]
  fn test_nested_ambiguous_prefixes() {
    assert_parameterized_parse_eq!(
      vec![],
      vec![("'", "#quote"), ("''", "#double-quote")],
      "'''a",
      Sexp::List(vec![
        Sexp::Leaf("#double-quote".to_string()),
        Sexp::List(vec![
          Sexp::Leaf("#quote".to_string()),
          Sexp::Leaf("a".to_string())
        ])
      ])
    );
  }

  #[test]
  fn test_parse_empty_symmetric_delimiter() {
    assert_parse_eq!("||", Sexp::List(vec![Sexp::Leaf("#pipe".to_string())]));
  }

  #[test]
  fn test_parse_filled_symmetric_delimiter() {
    assert_parse_eq!(
      "|x|",
      Sexp::List(vec![
        Sexp::Leaf("#pipe".to_string()),
        Sexp::Leaf("x".to_string())
      ])
    );
  }

  #[test]
  fn test_parse_symmetric_delimiter_inside_asymmetric_delimiter() {
    assert_parse_eq!(
      "(||)",
      Sexp::List(vec![Sexp::List(vec![Sexp::Leaf("#pipe".to_string()),]),])
    );
  }

  #[test]
  fn test_parse_multiple_symmetric_delimiters() {
    assert_parse_eq!(
      "(|| x ||)",
      Sexp::List(vec![
        Sexp::List(vec![Sexp::Leaf("#pipe".to_string())]),
        Sexp::Leaf("x".to_string()),
        Sexp::List(vec![Sexp::Leaf("#pipe".to_string())])
      ])
    );
  }

  #[test]
  fn test_parse_nested_symmetric_delimiters() {
    assert_parse_eq!(
      "|(|x|)|",
      Sexp::List(vec![
        Sexp::Leaf("#pipe".to_string()),
        Sexp::List(vec![Sexp::List(vec![
          Sexp::Leaf("#pipe".to_string()),
          Sexp::Leaf("x".to_string())
        ])])
      ])
    );
  }
}
