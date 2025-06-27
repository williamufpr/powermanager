import curses
def select_csv_file(csv_files):
        def menu(stdscr):
            curses.curs_set(0)
            current_row = 0
            while True:
                stdscr.clear()
                stdscr.addstr(0, 0, "Select a CSV file (use arrow keys and Enter):")
                for idx, file in enumerate(csv_files):
                    if idx == current_row:
                        stdscr.attron(curses.color_pair(1))
                        stdscr.addstr(idx + 2, 0, file)
                        stdscr.attroff(curses.color_pair(1))
                    else:
                        stdscr.addstr(idx + 2, 0, file)
                key = stdscr.getch()
                if key == curses.KEY_UP and current_row > 0:
                    current_row -= 1
                elif key == curses.KEY_DOWN and current_row < len(csv_files) - 1:
                    current_row += 1
                elif key in [curses.KEY_ENTER, 10, 13]:
                    return csv_files[current_row]
        curses.wrapper(lambda stdscr: curses.init_pair(1, curses.COLOR_BLACK, curses.COLOR_WHITE))
        return curses.wrapper(menu)
