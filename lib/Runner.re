open Message;

type response =
  | Bool(bool);

let do_ping = () => {
  Bool(true);
};

let do_nothing = () => {
  Bool(true);
};

let build_internal_command_thread = (command: Action.internal_command) => {
  switch (command) {
  | Ping => Lwt_result.return(do_ping())
  | Help => Lwt_result.return(do_nothing())
  };
};

let run = (action: Action.t) => {
  switch (action.runner) {
  | Internal(command) => build_internal_command_thread(command)
  };
};
