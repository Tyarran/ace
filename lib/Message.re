type input =
  | Shell(string);

type t = {input};

let make = (raw_input, origin: Types.Origin.t) => {
  switch (origin) {
  | Shell => {input: Shell(raw_input)}
  };
};
