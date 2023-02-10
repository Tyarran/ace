open Ace;
open Ace.Types;
open Alcotest;

module To_test = {
  let process_message = Ace.Processor.process_message;
};

/* tests */
let test_process_message_simple = () => {
  let message = Message.make("!ping", Origin.Shell);
  let actual = To_test.process_message(message);

  switch (actual) {
  | Ok(Command(name, arguments)) =>
    check(string, "should be ping", "ping", name);
    check(list(string), "should be an empty list", [], arguments);
  | Error(_) => failwith("should not be an error")
  };
};

let test_process_message_arguments = () => {
  let message = Message.make("!ping one two three", Origin.Shell);
  let actual = To_test.process_message(message);

  switch (actual) {
  | Ok(Command(name, arguments)) =>
    check(string, "should be ping", "ping", name);
    check(
      list(string),
      "should be have arguments",
      ["one", "two", "three"],
      arguments,
    );
  | Error(_) => failwith("should not be an error")
  };
};

let test_process_message_invalid_command = () => {
  let message = Message.make("ping one two three", Origin.Shell);
  let actual = To_test.process_message(message);

  switch (actual) {
  | Error(Processor.InvalidCommand(message)) =>
    check(string, "should be the message", "ping one two three", message)
  | _ => failwith("should not be an error")
  };
};

let test_process_message_empty_command = () => {
  let message = Message.make("", Origin.Shell);
  let actual = To_test.process_message(message);

  switch (actual) {
  | Error(Processor.InvalidCommand(message)) =>
    check(string, "should be the message", "", message)
  | _ => failwith("should not be an error")
  };
};

let test_process_message_double_exclamation = () => {
  let message = Message.make("!!ping", Origin.Shell);
  let actual = To_test.process_message(message);

  switch (actual) {
  | Error(Processor.InvalidCommandName(message)) =>
    check(string, "should be the message", "!ping", message)
  | _ => failwith("should not be an error")
  };
  Stdio.Out_channel.flush(Stdio.stdout);
};

let test_process_message_double_quoted_argument = () => {
  let message = Message.make("!ping \"quoted argument\"", Origin.Shell);
  let actual = To_test.process_message(message);

  switch (actual) {
  | Ok(Command(name, args)) =>
    check(string, "should be ping", "ping", name);
    check(
      list(string),
      "should contains one argument",
      ["quoted argument"],
      args,
    );
  | _ => failwith("should not be an error")
  };
  Stdio.Out_channel.flush(Stdio.stdout);
};

let () =
  run(
    "Processor",
    [
      (
        "process_message",
        [
          test_case("simple case", `Quick, test_process_message_simple),
          test_case("with arguments", `Quick, test_process_message_arguments),
          test_case(
            "with invalid command (missing \"!\")",
            `Quick,
            test_process_message_invalid_command,
          ),
          test_case(
            "with empty command",
            `Quick,
            test_process_message_empty_command,
          ),
          test_case(
            "with invalid command name (double \"!\")",
            `Quick,
            test_process_message_double_exclamation,
          ),
          test_case(
            "with double quoted argument",
            `Quick,
            test_process_message_double_quoted_argument,
          ),
        ],
      ),
    ],
  );
