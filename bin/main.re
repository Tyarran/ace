open Ace;
open Base;
open Stdio;

let config =
  Message.Config.{
    default_action:
      Message.Action.make(
        "help",
        None,
        Internal(Help),
        Command("help"),
        (),
      ),
    actions: [
      Message.Action.make(
        "ping",
        None,
        Internal(Ping),
        Command("ping"),
        (),
      ),
      Message.Action.make(
        "debug",
        None,
        Internal(Debug),
        Command("debug"),
        (),
      ),
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
          print_endline(Widget.render(response));
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
