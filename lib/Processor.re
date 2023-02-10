open Base;

type input =
  | Command(string, list(string));

type processor_error =
  | InvalidCommand(string);

module ShellProcessor = {
  type message = string;

  let parse_command = raw_command => {
    switch (String.split(raw_command, ~on=' ')) {
    | [] => Error(InvalidCommand(raw_command))
    | [name] => Ok(Command(name, []))
    | [name, ...args] => Ok(Command(name, args))
    };
  };

  let process_message = (message: message) => {
    switch (String.chop_prefix(message, ~prefix="!")) {
    | Some(raw_command) => parse_command(raw_command)
    | None => Error(InvalidCommand(message))
    };
  };
};

let process_message = (message: Message.t) => {
  switch (message.input) {
  | Message.Shell(raw_message) => ShellProcessor.process_message(raw_message)
  };
};
