open Base;

module Input: {
  type t;
  let get_raw: t => string;
  let from_shell: string => t;
};

type processor_error =
  | InvalidCommand(string)
  | InvalidCommandName(string);

module Action: {
  type internal_command =
    | Help
    | Debug
    | Ping;

  type runner =
    | Internal(internal_command);

  type trigger =
    | Command(string)
    | Unknown;

  type provider =
    | Shell
    | Slack;

  type destination =
    | Provider(provider)
    | SameAsOrigin;

  type t = {
    name: string,
    only_from: option(list(provider)),
    runner,
    trigger,
    destination,
  };

  let make:
    (
      string,
      option(list(provider)),
      runner,
      trigger,
      ~destination: destination=?,
      unit
    ) =>
    t;
};

module Config: {
  type t = {
    actions: list(Action.t),
    default_action: Action.t,
  };
};

type message_type =
  | Command(string, list(string));

type t = {
  input: Input.t,
  value: message_type,
};

let process: Input.t => Result.t(t, processor_error);
let find_action: (Config.t, t) => Action.t;
