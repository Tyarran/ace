open Base;
open Message;

type list_key_value_response = {
  intro: string,
  values: list((string, string)),
};

type response =
  | Bool(bool)
  | ListKeyValue(list_key_value_response);

let do_ping = () => {
  Bool(true);
};

let do_help = () => {
  let values =
    List.map([Action.Ping, Action.Help], ~f=command => {
      switch (command) {
      | Ping => ("ping", "Ping the bot")
      | Help => ("help", "Show this help")
      }
    });
  ListKeyValue({intro: "Available commands:", values});
};

let build_internal_command_thread = (command: Action.internal_command) => {
  switch (command) {
  | Ping => Lwt_result.return(do_ping())
  | Help => Lwt_result.return(do_help())
  };
};

let run = (action: Action.t) => {
  switch (action.runner) {
  | Internal(command) => build_internal_command_thread(command)
  };
};
