open Extended;

module Loggage = {
  type t = {style: string};

  let compare = (x, y) => String.compare(x.style, y.style);
};

module Tracker = {
  type edge = {
    max: int,
    from: Loggage.t,
    to_: Loggage.t,
  };

  type t = {
    nodes: list(Loggage.t),
    edges: list(edge),
  };

  let make = () => {nodes: [], edges: []};

  let append = (node, edges, tracker) => {
    nodes: tracker.nodes |> List.append([node]),
    edges: tracker.edges |> List.append(edges),
  };

  let incident_edges = (node, tracker) =>
    tracker.edges |> List.filter(edge => edge.from == node);

  let reverse_edge = edge => {...edge, from: edge.to_, to_: edge.from};
  let reverse = tracker => {
    ...tracker,
    edges: tracker.edges |> List.map(reverse_edge),
  };

  let find_paths = (node, tracker) => {
    let mark = (edge, discovered) => discovered |> List.append([edge]);

    let not_included_in = (edges, edge) => {
      let edge = edges |> List.find_opt(edges => edges == edge);
      edge == None;
    };

    // My first hand-written depth-first graph traversal brought from [Wikipedia](https://en.wikipedia.org/wiki/Graph_traversal#Depth-first_search)
    let rec traverse = (~edges, ~f, ~discovered=[], tracker) =>
      edges
      |> List.iter(edge => {
           // 1. evaluate edge
           f(edge);
           // 2. mark as discovered
           let discovered = discovered |> mark(edge);
           // 3. continue to traverse incident edges
           tracker
           |> traverse(
                ~edges=
                  tracker
                  |> incident_edges(edge.to_)
                  |> List.filter(not_included_in(discovered)),
                ~f,
                ~discovered,
              );
         });

    let paths = ref([]);
    let track = edge => paths := paths^ |> List.append([edge]);
    let edges = tracker |> incident_edges(node);
    tracker |> traverse(~edges, ~f=track);
    paths^;
  };

  let dedup_paths = edges =>
    edges |> List.sort_uniq((a, b) => Loggage.compare(a.to_, b.to_));
};

module Parser = {
  open Angstrom;

  include Util.Parser;

  let style_token = take_while1(Token.is_letter);

  let loggage =
    lift2(
      (token1, token2) => Loggage.{style: token1 ++ " " ++ token2},
      style_token <* space,
      style_token,
    );

  let sep = lex(char(',')) >>| ignore;
  let rule_no_bags = string("no other bags") >>| (_ => []);
  let rule_has_one =
    string("1 ") *> loggage <* string(" bag") >>| (loggage => (1, loggage));
  let rule_has_others =
    lift2(
      (count, loggage) => (count, loggage),
      digits <* space,
      loggage <* string(" bags"),
    );

  let rules_start = string("bags contain ") >>| ignore;
  let rules_end = char('.') >>| ignore;
  let rules = rule_no_bags <|> sep_by1(sep, rule_has_one <|> rule_has_others);

  let decl =
    lift2(
      (loggage, rules) =>
        (
          loggage,
          rules
          |> List.map(((max, other)) =>
               Tracker.{max, from: loggage, to_: other}
             ),
        ),
      loggage <* space,
      rules_start *> rules <* rules_end,
    );

  let parse_line = parse_string(~consume=All, decl);

  let parse = lines =>
    lines
    |> List.map(parse_line)
    |> List.mapi((i, result) =>
         switch (result) {
         | Ok(result) => result
         | Error(message) =>
           failwith("error at line " ++ string_of_int(i + 1) ++ message)
         }
       )
    |> List.fold_left(
         (tracker, (loggage, rules)) =>
           tracker |> Tracker.append(loggage, rules),
         Tracker.make(),
       );
};

let part1 = input => {
  let tracker = input |> Parser.parse;
  let target = Loggage.{style: "shiny gold"};

  Tracker.(
    tracker
    |> Tracker.reverse
    |> Tracker.find_paths(target)
    |> Tracker.dedup_paths
    |> List.length
    |> string_of_int
  );
};

let part2 = input => {
  let tracker = input |> Parser.parse;
  let target = Loggage.{style: "shiny gold"};
  let paths = Tracker.(tracker |> find_paths(target));

  let rec count_on_path = (at, paths) => {
    open Tracker;

    let (edges, paths) = paths |> List.partition(edge => edge.from == at);

    edges
    |> Tracker.dedup_paths
    |> List.map(edge => edge.max + edge.max * count_on_path(edge.to_, paths))
    |> List.fold_left((+), 0);
  };

  paths |> count_on_path(target) |> string_of_int;
};
