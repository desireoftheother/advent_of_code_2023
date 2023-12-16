from enum import Enum
from functools import partial
from typing import Any, Callable, Iterable

from consts import SPELLING_TO_DIGITS_MAPPING


class Digits(Enum):
    """Simple Enum class for better encapsulation of digits identification logic"""

    TENS = 0
    ONES = -1


def get_digit(
    source_dict: dict[str, list[Any]],
    spelling_to_digits_mapping: dict[str, int],
    digit: Digits,
) -> int:
    """Searches key with shortest first or last item in values"""
    return spelling_to_digits_mapping[
        min(
            source_dict.keys(),
            key=lambda subs: len(source_dict[subs][digit.value]),
        )
    ]


def decode_calibration_data(
    line: str, spelling_to_digits_mapping: dict[str, int]
) -> int:
    """Main hypothesis: part 1 is a filtering problem. Part 2 is a search problem.
    For solving it in a pythonic way we shall do following:
    1. For each spelling from dict we will split our string by it.
    2. Our first number will be the spelling which will have the shortest FIRST item in this split result.
    3. Our second number will be the spelling which will have the shortest LAST item in this split result."""

    spellings_to_splits: dict[str, list[str]] = {
        spelling: line.split(spelling) for spelling in spelling_to_digits_mapping.keys()
    }

    first_and_last_parts: dict[str, tuple[str, str]] = {
        spelling: (splits[0], splits[-1])
        for spelling, splits in spellings_to_splits.items()
    }

    get_digits_from_mapping: Callable[[Digits], int] = partial(
        get_digit,
        source_dict=first_and_last_parts,
        spelling_to_digits_mapping=spelling_to_digits_mapping,
    )

    tens: int = 10 * get_digits_from_mapping(digit=Digits.TENS)

    ones: int = get_digits_from_mapping(digit=Digits.ONES)

    return tens + ones


def main(
    file_name: str = "input.txt",
    spelling_to_digits_mapping: dict[str, int] = SPELLING_TO_DIGITS_MAPPING,
) -> int:
    with open(file_name, "r") as input_data:
        callibration_data: list[str] = input_data.readlines()

    calibration_values: Iterable[int] = (
        decode_calibration_data(
            line=line, spelling_to_digits_mapping=spelling_to_digits_mapping
        )
        for line in callibration_data
    )

    sum_of_calibration_values: int = sum(calibration_values)

    return sum_of_calibration_values


if __name__ == "__main__":
    print(main())
