open Base;
open Message;

type list_key_value_response = {
  intro: string,
  values: list((string, string)),
};

type debug = {
  raw: string,
  input: Message.Input.t,
  message: Message.t,
  action: Action.t,
};

type response =
  | Bool(bool)
  | ListKeyValue(list_key_value_response)
  | Debug(debug);

type context = {
  message: Message.t,
  action: Message.Action.t,
  config: Message.Config.t,
};

let do_ping = _input => {
  Bool(true);
};

let process_command = (raw, ctx) => {
  let input = Message.Input.from_shell(raw);
  switch (Message.process(input)) {
  | Ok(message) =>
    let action = Message.find_action(ctx.config, message);
    Ok(Debug({raw, message, input, action}));
  | Error(_) => Error("parsing error")
  };
};

let do_debug = (ctx: context) => {
  switch (ctx.message.value) {
  | Message.Command(_name, []) => Error("No command given")
  | Message.Command(_name, [raw]) => process_command(raw, ctx)
  | Message.Command(_name, [raw, ..._]) => process_command(raw, ctx)
  };
};

let do_help = _input => {
  let values =
    List.map([Action.Ping, Action.Help], ~f=command => {
      switch (command) {
      | Ping => ("ping", "Ping the bot")
      | Help => ("help", "Show this help")
      | Debug => ("debug", "Show debug information of a command")
      }
    });
  ListKeyValue({intro: "Available commands:", values});
};

let build_internal_command_thread = (command, ctx: context) => {
  switch (command) {
  | Message.Action.Ping => Lwt_result.return(do_ping(ctx))
  | Message.Action.Help => Lwt_result.return(do_help(ctx))
  | Message.Action.Debug => Lwt.return(do_debug(ctx))
  };
};

let build_context =
    (message: Message.t, config: Message.Config.t, action: Message.Action.t) => {
  config,
  message,
  action,
};

let run = (config, message, action) => {
  let ctx = build_context(message, config, action);
  switch (ctx.action.runner) {
  | Internal(command) => build_internal_command_thread(command, ctx)
  };
};
