from typing import Iterable


def main(file_name: str = "input.txt") -> int:
    with open(file_name, "r") as input_data:
        callibration_data: list[str] = input_data.readlines()

    clean_calibration_data: Iterable[list[int]] = (
        [int(symbol) for symbol in line_of_calib_data if symbol.isnumeric()]
        for line_of_calib_data in callibration_data
    )

    calibration_values: Iterable[int] = (
        10 * num_syms[0] + num_syms[-1] for num_syms in clean_calibration_data
    )

    sum_of_calibration_values: int = sum(calibration_values)

    return sum_of_calibration_values


if __name__ == "__main__":
    print(main())
