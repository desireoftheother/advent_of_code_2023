from typing import Self, Iterable, NamedTuple


class Record(NamedTuple):
    count_red: int
    count_green: int
    count_blue: int

    @classmethod
    def create_record_from_raw_line(cls, record_line: str) -> Self:
        lowered_line: str = record_line.lower()
        raw_line_stripped: str = lowered_line.strip()
        raw_list_of_pairs: list[str] = raw_line_stripped.split(", ")
        splitted_list_of_paris: Iterable[list[str]] = (
            pair.split(" ") for pair in raw_list_of_pairs
        )
        dict_of_colors: dict[str, int] = {
            pair[1]: int(pair[0]) for pair in splitted_list_of_paris
        }
        return cls(
            count_red=dict_of_colors.get("red", 0),
            count_blue=dict_of_colors.get("blue", 0),
            count_green=dict_of_colors.get("green", 0),
        )


class Game(NamedTuple):
    id: int
    records: list[Record]

    @classmethod
    def create_game_from_raw_line(cls, game_line: str) -> Self:
        id_and_games: list[str] = game_line.split(":")
        id: str = id_and_games[0].split(" ")[1]
        records_raw_list: str = id_and_games[1]
        records_raw_list_stripped: str = records_raw_list.strip()
        list_of_raw_records: list[str] = records_raw_list_stripped.split(";")
        records: list[Record] = [
            Record.create_record_from_raw_line(record_line=record_line)
            for record_line in list_of_raw_records
        ]
        return Game(id=id, records=records)


class GameAnalyzer(NamedTuple):
    raw_data: list[str]
    bag: Record = Record(count_red=12, count_green=13, count_blue=14)

    @classmethod
    def intialize_analyzer_with_input(cls, input_file_name) -> Self:
        with open(input_file_name, "r") as file:
            raw_data = file.readlines()
        return GameAnalyzer(raw_data=raw_data)

    def parse_raw_data(self) -> Iterable[Game]:
        return (
            Game.create_game_from_raw_line(game_line=line) for line in self.raw_data
        )

    def calculate_max_counts_per_game(self, all_games) -> dict[int, Record]:
        return {
            game.id: Record(
                count_blue=max(game.records, key=lambda rec: rec.count_blue).count_blue,
                count_red=max(game.records, key=lambda rec: rec.count_red).count_red,
                count_green=max(
                    game.records, key=lambda rec: rec.count_green
                ).count_green,
            )
            for game in all_games
        }

    def calculate_sum_of_ids_for_possible_games(self) -> int:
        all_games: Iterable[Game] = self.parse_raw_data()
        max_counts: dict[int, Record] = self.calculate_max_counts_per_game(all_games)
        possible_games: Iterable[int] = (
            int(id)
            for id, max_record in max_counts.items()
            if (
                max_record.count_blue <= self.bag.count_blue
                and max_record.count_red <= self.bag.count_red
                and max_record.count_green <= self.bag.count_green
            )
        )
        sum_of_ids: int = sum(possible_games)

        return sum_of_ids

    def calculate_sum_of_powers(self) -> int:
        all_games: Iterable[Game] = self.parse_raw_data()
        max_counts: dict[int, Record] = self.calculate_max_counts_per_game(all_games)
        max_counts_prod: Iterable[int] = (
            record.count_blue * record.count_red * record.count_green
            for record in max_counts.values()
        )

        sum_of_powers: int = sum(max_counts_prod)

        return sum_of_powers


if __name__ == "__main__":

    game_analyzer: GameAnalyzer = GameAnalyzer.intialize_analyzer_with_input(
            input_file_name="input.txt"
        )

    print(
        game_analyzer.calculate_sum_of_ids_for_possible_games()
    )

    print(
        game_analyzer.calculate_sum_of_powers()
    )
