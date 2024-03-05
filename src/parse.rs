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
enum Opening {
  Prefix,
  List(&'static [char], &'static [char]),
}
struct ParserState {
  expression_stack: Vec<Vec<Sexp>>,
  opening_stack: Vec<(usize, Opening)>,
}
impl ParserState {
  pub fn new() -> ParserState {
    ParserState {
      expression_stack: vec![vec![]],
      opening_stack: vec![],
    }
  }
  fn peek_opening(&mut self) -> Option<&Opening> {
    match self.opening_stack.last() {
      Some((_, opening)) => Some(&opening),
      None => None,
    }
  }
  pub fn insert_token(&mut self, token: String) {
    self.expression_stack[self.opening_stack.len()].push(Sexp::Leaf(token));
  }
  pub fn open_prefix(&mut self, char_index: usize, tag: &String) {
    self.expression_stack.push(vec![]);
    self.opening_stack.push((char_index, Opening::Prefix));
    self.insert_token(tag.clone());
  }
  pub fn close_expression(&mut self) {
    let top_expression = self.expression_stack.pop().unwrap();
    let expression_count = self.expression_stack.len();
    self.expression_stack[expression_count - 1]
      .push(Sexp::List(top_expression));
  }
  pub fn close_prefixes(&mut self) {
    while let Some(next_opening) = self.peek_opening() {
      match next_opening {
        Opening::Prefix => {
          self.close_expression();
          self.opening_stack.pop();
        }
        Opening::List(_, _) => break,
      }
    }
  }
  pub fn open_list(
    &mut self,
    char_index: usize,
    opener: &'static [char],
    closer: &'static [char],
    tag: &Option<String>,
  ) {
    self.expression_stack.push(vec![]);
    self
      .opening_stack
      .push((char_index, Opening::List(opener, closer)));
    match tag.clone() {
      Some(delimiter_tag) => self.insert_token(delimiter_tag),
      None => (),
    }
  }
  pub fn close_list(&mut self) -> Option<&[char]> {
    self.close_prefixes();
    self.close_expression();
    let closer = match self.opening_stack.pop() {
      Some(index_opening_pair) => match index_opening_pair.1 {
        Opening::List(_, closer) => Some(closer),
        Opening::Prefix => unreachable!(),
      },
      None => None,
    };
    self.close_prefixes();
    closer
  }
  pub fn get_open_list(&mut self) -> Option<(&[char], &[char])> {
    self
      .opening_stack
      .iter()
      .rev()
      .find_map(|index_opening_pair| match index_opening_pair.1 {
        Opening::Prefix => None,
        Opening::List(opener, closer) => Some((opener, closer)),
      })
  }
  pub fn finish(&mut self) -> Sexp {
    self
      .expression_stack
      .pop()
      .unwrap()
      .first()
      .unwrap()
      .clone()
  }
}

pub trait Delimiter {
  fn delimiter_strings(&self) -> (String, String);
  fn delimiter_tag(&self) -> Option<String>;
}

pub fn parse_chars<DelimiterType: Delimiter>(
  delimiters: &[DelimiterType],
  chars: Vec<char>,
) -> Result<Sexp, ParseError> {
  if chars.is_empty() {
    return Ok(Sexp::Leaf("nil".to_string()));
  }

  let delimiters: Vec<(&[char], &[char], Option<String>)> = vec![
    (&['('], &[')'], None),
    (&['['], &[']'], Some("#brackets".to_string())),
    (&['{'], &['}'], Some("#braces".to_string())),
    (&['#', '{'], &['}'], Some("#hash-braces".to_string())),
    (&['#', '['], &[']'], Some("#hash-brackets".to_string())),
  ];
  let prefixes: Vec<(&[char], String)> = vec![
    (&['\''], "quote".to_string()),
    (&['~'], "unquote".to_string()),
  ];

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
        chars_match(&prefixes[*prefix_index].0, &chars[char_index..])
      })
    } else {
      None
    };
    let matched_closer = delimiters
      .iter()
      .find(|(_, closer, _)| chars_match(closer, &chars[char_index..]));
    let open_list = parser_state.get_open_list();
    let was_expected_closer_matched = match open_list {
      Some((_, closer)) => chars_match(closer, &chars[char_index..]),
      None => false,
    };
    match matched_closer {
      None => (),
      Some(closer) => match open_list {
        None => {
          return Err(ParseError::UnmatchedCloser(closer.1.iter().collect()))
        }
        Some((opener, _)) => {
          if !was_expected_closer_matched {
            return Err(ParseError::MismatchedCloser(
              opener.iter().collect(),
              closer.1.iter().collect(),
            ));
          }
        }
      },
    }
    let matched_opening = delimiters
      .iter()
      .rev()
      .find(|delimiter| chars_match(&delimiter.0, &chars[char_index..]));
    if !was_expected_closer_matched && matched_closer.is_some() {}
    let whitespace = prefix_index.is_none()
      && matched_opening.is_none()
      && !was_expected_closer_matched
      && is_whitespace(char);
    if consumed_index != char_index
      && (string_opener
        || matched_opening.is_some()
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
        let prefix = &prefixes[i];
        parser_state.open_prefix(char_index, &prefix.1);
        prefix.0.len()
      }
      None => match matched_opening {
        Some(delimiter) => {
          expression_opened = true;
          // If this is an opener, add a list, with the tag of the delimiter
          // pair (if it has one) as the first element of the list, to the AST.
          parser_state.open_list(
            char_index,
            delimiter.0,
            delimiter.1,
            &delimiter.2,
          );
          delimiter.0.len()
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
      || matched_opening.is_some()
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
  return Ok(parser_state.finish());
}

pub fn parse<DelimiterType: Delimiter>(
  delimiters: &[DelimiterType],
  s: &str,
) -> Result<Sexp, ParseError> {
  parse_chars(delimiters, s.chars().collect())
}

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
    fn delimiter_strings(&self) -> (String, String) {
      match self {
        TestDelimiter::Parentheses => ("(".to_string(), ")".to_string()),
        TestDelimiter::Brackets => ("[".to_string(), "]".to_string()),
        TestDelimiter::Braces => ("{".to_string(), "}".to_string()),
        TestDelimiter::HashBrackets => ("#[".to_string(), "]".to_string()),
        TestDelimiter::HashBraces => ("#{".to_string(), "}".to_string()),
      }
    }

    fn delimiter_tag(&self) -> Option<String> {
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

  #[test]
  fn test_parse() {
    [
      ("()", Sexp::List(vec![])),
      ("[]", Sexp::List(vec![Sexp::Leaf("#brackets".to_string())])),
      ("{}", Sexp::List(vec![Sexp::Leaf("#braces".to_string())])),
      ("hello!", Sexp::Leaf("hello!".to_string())),
      (
        "(+ 1 2)",
        Sexp::List(vec![
          Sexp::Leaf("+".to_string()),
          Sexp::Leaf("1".to_string()),
          Sexp::Leaf("2".to_string()),
        ]),
      ),
      (
        "'(+ 1 2)",
        Sexp::List(vec![
          Sexp::Leaf("quote".to_string()),
          Sexp::List(vec![
            Sexp::Leaf("+".to_string()),
            Sexp::Leaf("1".to_string()),
            Sexp::Leaf("2".to_string()),
          ]),
        ]),
      ),
      (
        "~'()",
        Sexp::List(vec![
          Sexp::Leaf("unquote".to_string()),
          Sexp::List(vec![Sexp::Leaf("quote".to_string()), Sexp::List(vec![])]),
        ]),
      ),
      (
        "'a",
        Sexp::List(vec![
          Sexp::Leaf("quote".to_string()),
          Sexp::Leaf("a".to_string()),
        ]),
      ),
      (
        "''a",
        Sexp::List(vec![
          Sexp::Leaf("quote".to_string()),
          Sexp::List(vec![
            Sexp::Leaf("quote".to_string()),
            Sexp::Leaf("a".to_string()),
          ]),
        ]),
      ),
    ]
    .into_iter()
    .for_each(|(str, sexp)| {
      match parse(&TEST_DELIMITERS, str) {
        Ok(parsed_sexp) => assert_eq!(
          sexp, parsed_sexp,
          "String {:?} was not parsed as expected.",
          str
        ),
        Err(e) => {
          assert!(false, "Failed to parse string {:?}, got error {:?}", str, e)
        }
      };
    });
  }
}
