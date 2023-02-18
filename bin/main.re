open Ace;
open Base;
open Stdio;

let config =
  Message.Config.{
    default_action:
      Message.Action.{
        name: "help",
        only_from: None,
        runner: Internal(Help),
        trigger: Command("help"),
      },
    actions: [
      Message.Action.{
        name: "ping",
        only_from: None,
        runner: Internal(Ping),
        trigger: Command("ping"),
      },
      Message.Action.{
        name: "debug",
        only_from: None,
        runner: Internal(Debug),
        trigger: Command("debug"),
      },
    ],
  };

Stdio.print_endline("");

let handle_message = raw_message => {
  let input = Message.Input.from_shell(raw_message);
  let message_res = Message.process(input);
  switch (message_res) {
  | Ok(message) =>
    Lwt.(
      Message.find_action(config, message)
      |> Runner.run(config, message)
      >>= (
        response => {
          switch (response) {
          | Ok(Runner.Bool(value)) =>
            print_endline("received : " ++ Bool.to_string(value))
          | Ok(Runner.ListKeyValue(_value)) =>
            print_endline("received : command list")
          | Ok(Runner.Debug(debug)) =>
            print_endline("received : debug");
            print_endline("command : " ++ debug.raw);
          | Error(_error) => print_endline("Execution error")
          };
          Lwt.return_unit;
        }
      )
    )
  | Error(_) => Lwt.return_unit
  };
};

handle_message("!ping");
handle_message("!help");
handle_message("!skdljf;slakjf");
handle_message("!skdljfslakjf");
handle_message("!help one two three");
handle_message("help one two three four");
handle_message("!debug \"!ping\"");
