open Ace_lib.Types;
open Ace_lib;
open Base;
open Cohttp;
open Interface;
open Lwt;
open Minitel;
open React;
open Stdio;

type shell_result =
  | ShellContinue
  | ShellExit;

type shell_error =
  /* | SystemCommandFail(int) */
  | SystemCommandResult(string)
  | ProcessingError(Processor.processing_error);

exception SystemCommandFail(int);
exception CommandFail(Processor.processing_error);

let shell_commands = [|"exit", "quit", "clear"|];

class read_line (~term, ~history, ~exit_code, ~binaries, ~line_number) = {
  inherit (class LTerm_read_line.read_line)(~history, ());
  inherit (class LTerm_read_line.term(Zed_string.t))(term);
  pub! completion = {
    let prefix = Zed_rope.to_string(this#input_prev);
    let binaries =
      List.filter(binaries, ~f=binary => {
        Zed_string.starts_with(binary, ~prefix)
      });

    this#set_completion(
      0,
      List.map(binaries, ~f=binary =>
        (binary, Zed_string.unsafe_of_utf8(""))
      ),
    );
  };
  initializer (
    this#set_prompt(S.l1(size => Interface.prompt(line_number), this#size))
  );
};

let get_binaries = (config: Config.t, shell_commands) => {
  let actions_binaries =
    config.actions
    |> Array.to_list
    |> List.map(~f=(action: Action.t) => {
         List.filter_map(action.on, ~f=event =>
           switch (event) {
           | Event.Command(name) =>
             Some(Zed_string.unsafe_of_utf8("!" ++ name))
           | _other => None
           }
         )
       })
    |> List.concat;
  let shell_binaries =
    shell_commands
    |> Array.map(~f=command => Zed_string.unsafe_of_utf8(command))
    |> Array.to_list;

  List.concat([actions_binaries, shell_binaries]);
};

let execute_system_command = command_name =>
  command_name
  |> Lwt_process.shell
  |> Lwt_process.exec
  >>= (
    status => {
      switch (status) {
      | Unix.WEXITED(0) => Lwt.return()
      | Unix.WEXITED(return_code)
      | Unix.WSTOPPED(return_code)
      | Unix.WSIGNALED(return_code) =>
        Lwt.fail(SystemCommandFail(return_code))
      };
    }
  );

let execute = thread => {
  Lwt.(
    thread
    >>= (
      res =>
        switch (res) {
        | Ok(response) =>
          let chat_line_data = Widgets.render(response, Types.Service.Shell);
          switch (chat_line_data) {
          | ChatLine(context, username, lines) =>
            Interface.debug_info(context);
            <minitel>
              <chat_line color=Blue user=username> ...lines </chat_line>
            </minitel>
            |> Lwt_result.return;
          | _ => Lwt_result.return("")
          };
        | Error(error: shell_error) => Lwt_result.fail(error)
        }
    )
  );
};

let handle_input = (config: Config.t, raw_input) => {
  let input = In.Shell(raw_input);
  Processor.get_thread(
    config,
    input,
    ~default=(config.default_action, No_event),
  );
};

let print_result = res => {
  switch (res) {
  | Ok(res) => Lwt_io.write_line(Lwt_io.stdout, res)
  | Error(error) => Lwt_io.write_line(Lwt_io.stderr, "Error")
  };
};

let handle_input = (history, config, term_input) => {
  LTerm_history.add(history, term_input);
  switch (Zed_string.to_utf8(term_input)) {
  | "quit"
  | "exit" => Lwt.return(ShellExit)
  | "clear" =>
    execute_system_command("clear") >>= (_ => Lwt.return(ShellContinue))
  | command =>
    let _ =
      switch (handle_input(config, command)) {
      | Ok(thread) => execute(thread) >>= print_result
      | Error(error) => Lwt.fail(CommandFail(error))
      };
    Lwt.return(ShellContinue);
  };
};

let rec shell_loop =
        (~binaries, ~term, ~history, ~exit_code, ~line_number=1, ~config, ()) => {
  (new read_line)(
    ~term,
    ~history=LTerm_history.contents(history),
    ~exit_code,
    ~binaries,
    ~line_number,
  )#
    run
  >>= (term_input => handle_input(history, config, term_input))
  >>= (
    next =>
      switch (next) {
      | ShellExit => Lwt.return()
      | ShellContinue =>
        shell_loop(
          ~binaries,
          ~term,
          ~history,
          ~exit_code,
          ~line_number=line_number + 1,
          ~config,
          (),
        )
      }
  );
};

let start_shell = config => {
  let _ =
    Interface.welcome(config)
    |> Interface.success
    |> Interface.print
    |> Interface.flush;
  let loop =
    shell_loop(
      ~binaries=get_binaries(config, shell_commands),
      ~history=LTerm_history.create([]),
      ~exit_code=0,
      ~config,
    );
  let shell =
    LTerm_inputrc.load()
    >>= (_ => Lazy.force(LTerm.stdout) >>= (term => loop(~term, ())));

  let _ = Lwt_main.run(shell);
  ();
};

let run = () => {
  Message.success("Start Ace Shell", "") |> Interface.print;

  Message.success("Load configuration", "config.yaml") |> Interface.print;
  switch (ConfigParser.read_config_file_sync("config.yaml")) {
  | Ok(config) =>
    Message.success("Started bot", config.bot.name) |> Interface.print;
    Interface.flush();
    start_shell(config);
  | Error(msg) =>
    Message.error("Unable to read the config file", "") |> Interface.print
  };
  Interface.print("\r\n");
  Interface.flush();
};
