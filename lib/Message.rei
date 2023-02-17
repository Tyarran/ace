open Base;

type t;

type input =
  | Command(string, list(string));

type processor_error =
  | InvalidCommand(string)
  | InvalidCommandName(string);

module Action: {
  type internal_command =
    | Help
    | Ping;

  type runner =
    | Internal(internal_command);

  type trigger =
    | Command(string)
    | Unknown;

  type origin =
    | Shell
    | Slack;

  type t = {
    name: string,
    only_from: option(list(origin)),
    runner,
    trigger,
  };
};

let make_shell: string => t;
let parse: t => Result.t(input, processor_error);
let find_action: (list(Action.t), Action.t, t, input) => Action.t;
