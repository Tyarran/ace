open Ministel;
open Types;
open Base;

type response = string;

let message = (~value, arrow_color, step_color, value_color, step_text) => {
  <terminal>
    <line>
      <text color=step_color> {step_text ++ ": "} </text>
      <text color=value_color> value </text>
    </line>
  </terminal>;
};

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

let debug = (~step, ~value, ~children, ~user, ()) => {
  let message = message(~value, Yellow, Yellow, Yellow, step);
  <chat_line user color=Yellow> message </chat_line>;
};

let debug_info = (~context: Context.t, ~response, ~children=[], ()) =>
  if (context.config.bot.debug) {
    let user =
      Utils.expand_name("debug", [context.config.bot.name, "debug", "you"]);
    <terminal>
      <chat_line user color=Yellow>
        <text color=Yellow> {"botname: " ++ context.config.bot.name} </text>
        <text color=Yellow> {"input: " ++ In.get_text(context.input)} </text>
        <text color=Yellow>
          {"origin: " ++ In.get_service_name(context.input)}
        </text>
        <text color=Yellow> {"action: " ++ context.action.name} </text>
        <text color=Yellow>
          {"event: " ++ Event.to_string(context.event)}
        </text>
        <text color=Yellow>
          {"runner: " ++ Action.Runner.to_string(context.action.runner)}
        </text>
        <text color=Yellow> "destination: shell" </text>
        <text color=Yellow> "response :response" </text>
      </chat_line>
    </terminal>;
  } else {
    "";
  };

let render_shell = (context: Context.t, response) => {
  let botname =
    Utils.expand_name(
      context.config.bot.name,
      [context.config.bot.name, "you", "debug"],
    );
  <terminal>

      <chat_line user=botname color=Blue>
        <text> {"Action: " ++ context.action.name} </text>
        <text> "result: " </text>
        <text> response </text>
      </chat_line>
    </terminal>;
    /* <debug_info context response /> */
};
