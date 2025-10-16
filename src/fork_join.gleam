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
  NOP
  Counter(String, Int)
  Label(String, Token)
  Fork(String)
  Goto(String)
  Join(String, String)
  Literal(String)
  Quit
  LexError(String)
}

fn lex_counter(line_num: Int, line: String) {
  case string.split(line, "=") {
    [name, value_str] ->
      case int.parse(value_str) {
        Ok(value) -> Counter(name, value)
        _ ->
          LexError(
            "Tipo incorrecto en linea"
            <> int.to_string(line_num)
            <> ", un contador solo puede guardar números enteros. ('"
            <> line
            <> "')",
          )
      }
    _ ->
      LexError(
        "Error de sintaxis en linea"
        <> int.to_string(line_num)
        <> ", esperaba la asignación de un contador pero obtuve '"
        <> line
        <> "'",
      )
  }
}

fn lex_line(line_num: Int, line: List(String)) {
  case line {
    [] | ["QUIT"] -> Quit
    ["FORK", label] -> Fork(label)
    ["GOTO", label] -> Goto(label)
    ["JOIN", counter, label] -> Join(counter, label)
    [str] ->
      case string.contains(str, "=") {
        True -> lex_counter(line_num, str)
        False ->
          case string.ends_with(str, ":") {
            True -> Label(string.drop_end(str, 1), NOP)
            False -> Literal(str)
          }
      }
    [label, ..rest] ->
      case string.contains(label, ":") {
        True -> Label(string.drop_end(label, 1), lex_line(line_num, rest))
        False ->
          LexError(
            "Error de sintaxis en linea "
            <> int.to_string(line_num)
            <> ", no esperaba '"
            <> string.join(line, " ")
            <> "'",
          )
      }
  }
}

fn lex(source: String) {
  let s =
    string.uppercase(source)
    |> string.split(on: "\n")
    |> list.filter_map(fn(s) {
      let ts = string.trim(s)
      case ts {
        "--" <> _ -> Error(Nil)
        _ ->
          case string.is_empty(ts) {
            True -> Error(Nil)
            False -> Ok(ts)
          }
      }
    })
  list.map2(list.range(1, list.length(s)), s, fn(i, s) {
    string.split(s, on: " ")
    |> list.filter(compose(bool.negate, string.is_empty))
    |> lex_line(i, _)
  })
}

fn parse(parent: Option(String), tokens: List(Token), source: List(Token)) {
  let merge = fn(a, b) { list.append(a, b) |> list.unique }
  case tokens {
    [] -> dict.new()
    [t, ..ts] ->
      case t {
        NOP | Counter(_, _) -> parse(parent, ts, source)
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
        LexError(e) -> panic as e
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
