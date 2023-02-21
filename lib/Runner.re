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

let do_ping = (ctx: Context.t) => {
  (Response.(Ok(CommandResult("pong"))), Context.get_destination(ctx));
};

let process_command = (raw, ctx: Context.t) => {
  let input = Message.Input.from_shell(raw);
  switch (Message.process(input)) {
  | Ok(message) =>
    let action = Message.find_action(ctx.config, message);
    (
      Response.(Ok(Debug({raw, message, input, action}))),
      Context.get_destination(ctx),
    );
  | Error(_) => (Error("parsing error"), Context.get_destination(ctx))
  };
};

let do_debug = (ctx: Context.t) => {
  switch (ctx.message.value) {
  | Message.Command(_name, []) => (
      Error("No command given"),
      Context.get_destination(ctx),
    )
  | Message.Command(_name, [raw]) => process_command(raw, ctx)
  | Message.Command(_name, [raw, ..._]) => process_command(raw, ctx)
  };
};

let do_help = (ctx: Context.t) => {
  let values =
    List.map([Action.Ping, Action.Help], ~f=command => {
      switch (command) {
      | Ping => ("ping", "Ping the bot")
      | Help => ("help", "Show this help")
      | Debug => ("debug", "Show debug information of a command")
      }
    });
  (
    Response.(
      Ok(
        UserManualResponse({
          intro: None,
          title: Some("Available commands:"),
          values,
        }),
      )
    ),
    Context.get_destination(ctx),
  );
};

let build_internal_command_thread = (command, ctx: Context.t) => {
  switch (command) {
  | Message.Action.Ping => Lwt.return(do_ping(ctx))
  | Message.Action.Help => Lwt.return(do_help(ctx))
  | Message.Action.Debug => Lwt.return(do_debug(ctx))
  };
};

let run = (config, message, action) => {
  let ctx = Context.make(message, config, action);
  switch (ctx.action.runner) {
  | Internal(command) => build_internal_command_thread(command, ctx)
  };
};
