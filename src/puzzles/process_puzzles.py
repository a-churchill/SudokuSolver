"""
Expects the puzzles to be in blocks of text, separated by blank lines; first line is the
unsolved puzzle, second line is the solved puzzle, and the remaining lines in the block
are ignored.
"""
with open("all.txt", "r") as all_puzzles:
    puzzle_num = 1
    parse_state = 1
    for line in all_puzzles:
        if parse_state == 1:
            with open(str(puzzle_num), "w") as unsolved:
                unsolved.write(line.strip("\n"))
            parse_state += 1
        elif parse_state == 2:
            with open(str(puzzle_num) + "_sol", "w") as solved:
                solved.write(line.strip("\n"))
            parse_state += 1
        if len(line) == 1:  # found new line, separating puzzles
            parse_state = 1
            puzzle_num += 1
    print("Wrote {0} puzzles".format(puzzle_num))
