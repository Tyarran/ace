open Ace;
open Base;
open Stdio;

let default_action =
  Message.Action.{
    name: "help",
    only_from: None,
    runner: Internal(Help),
    trigger: Command("help"),
  };

let actions = [
  Message.Action.{
    name: "ping",
    only_from: None,
    runner: Internal(Ping),
    trigger: Command("ping"),
  },
  default_action,
];

Stdio.print_endline("");

let handle_message = raw_message => {
  let message = Message.make_shell(raw_message);
  let action_finder = Message.find_action(actions, default_action);

  let read_response = response => {
    switch (response) {
    | Ok(Runner.Bool(value)) =>
      print_endline("received : " ++ Bool.to_string(value))
    | Ok(Runner.ListKeyValue(_value)) =>
      print_endline("received : command list")
    | _ => print_endline("execution error")
    };
    Lwt.return_unit;
  };

  switch (Message.parse(message)) {
  | Ok(input) =>
    let thread = input |> action_finder(message) |> Runner.run;
    Lwt.(Lwt_main.run(thread >>= read_response));
  | _ => print_endline("Error")
  };
};

handle_message("!ping");
handle_message("!help");
handle_message("!skdljf;slakjf");
handle_message("!skdljfslakjf");
handle_message("!help one two three");
handle_message("help one two three four");
