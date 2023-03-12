open Ace_lib;
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

let shell = () => {
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

  let _ = handle_message("!ping");
  let _ = handle_message("!help");
  let _ = handle_message("!skdljf;slakjf");
  let _ = handle_message("!skdljfslakjf");
  let _ = handle_message("!help one two three");
  let _ = handle_message("help one two three four");
  let _ = handle_message("!debug \"!ping\"");
  ();
};

type command =
  | Server
  | Shell;

let () = {
  Stdio.print_endline("");

  Clap.description("ACE -Another Chatbot Engine- executable.");
  let command =
    Clap.subcommand([
      Clap.case(~description="Start ace server", "start", () => Server),
      Clap.case(~description="Enter in interactive shell", "shell", () =>
        Shell
      ),
    ]);
  switch (command) {
  | Server => Stdio.print_endline("Starting server...")
  | Shell => shell()
  };
  Clap.close();
};
