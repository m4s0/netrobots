'''
    Copyright 2015, 2016 Roberto Gambuzzi <gbinside@gmail.com>
    Copyright 2015, 2016 Massimo Zaniboni <massimo.zaniboni@docmelody.com>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
'''

from tornado.options import define, options, parse_command_line,  print_help
from server.game_server import run_server

define("run", help="run the server on the specified http port")
define("board_x_size", default="1000")
define("board_y_size", default="1000")
define("network_latency", default="0.100", help="The network latency in seconds.")
define("game_tick", default="0.25", help="The virtual simulation time in seconds, between two consecutive robot commands.")

parse_command_line()

if not options.run:
    # TODO print license
    print_help()
else:
    run_server(options.run, int(float(options.board_x_size)), int(float(options.board_y_size)), float(options.network_latency), float(options.game_tick))
