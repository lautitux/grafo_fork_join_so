import gleam/bool
import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/string

fn compose(fun1: fn(b) -> c, fun2: fn(a) -> b) -> fn(a) -> c {
  fn(arg) { fun1(fun2(arg)) }
}

pub type Token {
  Comment
  Counter(String, Int)
  Label(String, Token)
  Fork(String)
  Goto(String)
  Join(String, String)
  Literal(String)
  Quit
  Error(String)
}

fn lex_counter(line: String) {
  case string.split(line, "=") {
    [name, value_str] ->
      case int.parse(value_str) {
        Ok(value) -> Counter(name, value)
        _ ->
          Error(
            "Tipo incorrecto, solo se le pueden asignar números enteros a un contador",
          )
      }
    _ ->
      Error(
        "Error de sintaxis, esperaba la asignación de un contador pero obtuve '"
        <> line
        <> "'",
      )
  }
}

fn lex_line(line: List(String)) {
  case line {
    [] | ["QUIT"] -> Quit
    ["FORK", label] -> Fork(label)
    ["GOTO", label] -> Goto(label)
    ["JOIN", counter, label] -> Join(counter, label)
    ["--" <> _, ..] -> Comment
    [str] ->
      case string.contains(str, "=") {
        True -> lex_counter(str)
        False -> Literal(str)
      }
    [label, ..rest] ->
      case string.contains(label, ":") {
        True -> Label(string.drop_end(label, 1), lex_line(rest))
        False ->
          Error(
            "Error de sintaxis, no esperaba '" <> string.join(line, " ") <> "'",
          )
      }
  }
}

fn lex(source: String) {
  string.uppercase(source)
  |> string.split(on: "\n")
  |> list.map(string.trim)
  |> list.filter(compose(bool.negate, string.is_empty))
  |> list.map(fn(s) {
    string.split(s, on: " ")
    |> list.filter(compose(bool.negate, string.is_empty))
    |> lex_line
  })
}

fn parse(parent: Option(String), tokens: List(Token), source: List(Token)) {
  let merge = fn(a, b) { list.append(a, b) |> list.unique }
  case tokens {
    [] -> dict.new()
    [t, ..ts] ->
      case t {
        Comment | Counter(_, _) -> parse(parent, ts, source)
        Quit -> dict.new()
        Label(_, lt) -> parse(parent, list.append([lt], ts), source)
        Literal(l) -> {
          case parent {
            option.None ->
              dict.combine(
                parse(option.Some(l), ts, source),
                dict.from_list([#(l, [])]),
                merge,
              )
            option.Some(parent) ->
              dict.combine(
                parse(option.Some(l), ts, source),
                dict.from_list([#(l, []), #(parent, [l])]),
                merge,
              )
          }
        }
        Goto(goto_label) ->
          parse(
            parent,
            list.drop_while(source, fn(t) {
              case t {
                Label(label, _) -> label != goto_label
                _ -> True
              }
            }),
            source,
          )
        Fork(fork_label) ->
          dict.combine(
            parse(
              parent,
              list.drop_while(source, fn(t) {
                case t {
                  Label(label, _) -> label != fork_label
                  _ -> True
                }
              }),
              source,
            ),
            parse(parent, ts, source),
            merge,
          )
        Join(_, join_label) ->
          dict.combine(
            parse(parent, ts, source),
            parse(
              parent,
              list.drop_while(source, fn(t) {
                case t {
                  Label(label, _) -> label != join_label
                  _ -> True
                }
              }),
              source,
            ),
            merge,
          )
        Error(e) -> panic as e
      }
  }
}

pub fn make_mermaid_graph(g: dict.Dict(String, List(String))) {
  "flowchart TD\n"
  <> dict.to_list(g)
  |> list.map(fn(kv) {
    case list.is_empty(kv.1) {
      True -> "    " <> kv.0
      False -> "    " <> kv.0 <> " --> " <> string.join(kv.1, with: " & ")
    }
  })
  |> string.join(with: "\n")
}

pub fn make_graph(src: String) {
  let src = lex(src)
  parse(option.None, src, src)
}
