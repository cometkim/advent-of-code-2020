let iter_slope = (~slope_right: int, ~slope_down: int, f, l) => {
  let current_x = ref(slope_right);
  let current_y = ref(slope_down);
  l
  |> List.iteri((y, line) =>
       if (current_y^ == y) {
         let width = line |> String.length;
         f(line.[current_x^]);
         current_x := (current_x^ + slope_right) mod width;
         current_y := current_y^ + slope_down;
       }
     );
};

let part1 = input => {
  let count = ref(0);
  input
  |> iter_slope(~slope_right=3, ~slope_down=1, x => {
       switch (x == '#') {
       | true => count := count^ + 1
       | _ => ()
       }
     });
  string_of_int(count^);
};

let part2 = input => {
  let count1 = ref(0);
  input
  |> iter_slope(~slope_right=1, ~slope_down=1, x => {
       switch (x == '#') {
       | true => count1 := count1^ + 1
       | _ => ()
       }
     });

  let count2 = ref(0);
  input
  |> iter_slope(~slope_right=3, ~slope_down=1, x => {
       switch (x == '#') {
       | true => count2 := count2^ + 1
       | _ => ()
       }
     });

  let count3 = ref(0);
  input
  |> iter_slope(~slope_right=5, ~slope_down=1, x => {
       switch (x == '#') {
       | true => count3 := count3^ + 1
       | _ => ()
       }
     });

  let count4 = ref(0);
  input
  |> iter_slope(~slope_right=7, ~slope_down=1, x => {
       switch (x == '#') {
       | true => count4 := count4^ + 1
       | _ => ()
       }
     });

  let count5 = ref(0);
  input
  |> iter_slope(~slope_right=1, ~slope_down=2, x => {
       switch (x == '#') {
       | true => count5 := count5^ + 1
       | _ => ()
       }
     });

  string_of_int(count1^ * count2^ * count3^ * count4^ * count5^);
};
