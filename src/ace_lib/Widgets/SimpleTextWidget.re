open Base;
open Ministel;

type response = string;

let chat_line = (~color, ~user, ~children=[], ()) => {
  let (first, other_lines) =
    switch (children) {
    | [] => ("", "")
    | [child] => (<text> child </text>, "")
    | [first, ...rest] =>
      let space = Utils.space(user);
      let other_lines =
        rest
        |> List.map(~f=child => <line> {space ++ child} </line>)
        |> String.concat;
      (<text> first </text>, other_lines);
    };
  <terminal>
    <line> <text color> {user ++ " > "} </text> <text> first </text> </line>
    other_lines
  </terminal>;
};

let render_shell = (context: Types.Context.t, response) =>
  <terminal>
    <chat_line color=Blue user={context.config.bot.name}> response </chat_line>
  </terminal>;
