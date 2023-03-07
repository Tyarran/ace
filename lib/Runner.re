open Base;
open Message;
open Types;

module Context = {
  type t = {
    message: Message.t,
    action: Message.Action.t,
    config: Message.Config.t,
  };

  let make =
      (message: Message.t, config: Message.Config.t, action: Message.Action.t) => {
    config,
    message,
    action,
  };

  let get_destination = (ctx: t) => {
    switch (ctx.action.destination) {
    | Provider(provider) => provider
    | SameAsOrigin =>
      switch (ctx.message.input) {
      | Input.Shell(_) => Message.Action.Shell
      }
    };
  };
};

let do_ping = (_ctx: Context.t) => Ok("pong");

let process_command = (raw, ctx: Context.t) => {
  let input = Message.Input.from_shell(raw);
  switch (Message.process(input)) {
  | Ok(message) =>
    let action = Message.find_action(ctx.config, message);
    Ok(Response.{raw, message, input, action});
  | Error(_) => Error("parsing error")
  };
};

let do_debug = (ctx: Context.t) => {
  switch (ctx.message.value) {
  | Message.Command(_name, []) => Error("No command given")
  | Message.Command(_name, [raw]) => process_command(raw, ctx)
  | Message.Command(_name, [raw, ..._]) => process_command(raw, ctx)
  };
};

let do_help = (_ctx: Context.t) => {
  let values =
    List.map([Action.Ping, Action.Help], ~f=command => {
      switch (command) {
      | Action.Ping => ("ping", "Ping the bot")
      | Action.Help => ("help", "Show this help")
      | Action.Debug => ("debug", "Show debug information of a command")
      }
    });
  Ok(Response.{intro: None, title: Some("Available commands:"), values});
};

let build_internal_command_thread = (command, ctx: Context.t) => {
  let destination = Context.get_destination(ctx);
  switch (command) {
  | Message.Action.Ping =>
    Lwt.return(
      Response.{result: Ok(CommandResult(do_ping(ctx))), destination},
    )
  | Message.Action.Help =>
    Lwt.return(
      Response.{result: Ok(UserManualResponse(do_help(ctx))), destination},
    )
  | Message.Action.Debug =>
    Lwt.return(Response.{result: Ok(Debug(do_debug(ctx))), destination})
  };
};

let run = (config, message, action) => {
  let ctx = Context.make(message, config, action);
  switch (ctx.action.runner) {
  | Internal(command) => build_internal_command_thread(command, ctx)
  };
};
