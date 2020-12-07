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

let count_with_slope = (~slope_right, ~slope_down, l) => {
  let count = ref(0);
  l
  |> iter_slope(~slope_right, ~slope_down, x => {
       switch (x == '#') {
       | true => count := count^ + 1
       | _ => ()
       }
     });
  count^;
};

let part1 = input => {
  let count = input |> count_with_slope(~slope_right=3, ~slope_down=1);
  string_of_int(count);
};

let part2 = input => {
  let count1 = input |> count_with_slope(~slope_right=1, ~slope_down=1);
  let count2 = input |> count_with_slope(~slope_right=3, ~slope_down=1);
  let count3 = input |> count_with_slope(~slope_right=5, ~slope_down=1);
  let count4 = input |> count_with_slope(~slope_right=7, ~slope_down=1);
  let count5 = input |> count_with_slope(~slope_right=1, ~slope_down=2);
  Util.string_of_multiply([count1, count2, count3, count4, count5]);
};
