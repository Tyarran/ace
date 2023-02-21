open Types;

module CommandResultWidget = {
  let render_shell = value => value;

  let render = (value, destination: Message.Action.provider) => {
    switch (destination) {
    | Shell => render_shell(value)
    };
  };
};

module UserManualWidget = {
  let render_shell = _value => {
    "User manual response";
  };

  let render = (value, destination: Message.Action.provider) => {
    switch (destination) {
    | Shell => render_shell(value)
    };
  };
};

module DebugWidget = {
  let render_shell = _value => {
    "debug";
  };

  let render = (value, destination: Message.Action.provider) => {
    switch (destination) {
    | Shell => render_shell(value)
    };
  };
};

module ErrorWidget = {
  let render_shell = _value => {
    "error";
  };

  let render = (value, destination: Message.Action.provider) => {
    switch (destination) {
    | Shell => render_shell(value)
    };
  };
};

let render = (response: Response.t) => {
  switch (response) {
  | (Ok(CommandResult(value)), destination) =>
    CommandResultWidget.render(value, destination)
  | (Ok(Debug(value)), destination) =>
    DebugWidget.render(value, destination)
  | (Ok(UserManualResponse(value)), destination) =>
    UserManualWidget.render(value, destination)
  | (Error(value), destination) => ErrorWidget.render(value, destination)
  };
};
