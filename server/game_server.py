'''
    Copyright 2015, 2016 Roberto Gambuzzi <gbinside@gmail.com>
    Copyright 2015, 2016 Massimo Zaniboni <massimo.zaniboni@gmail.com>

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

import tornado.ioloop
import tornado.web
from tornado import gen
from tornado.concurrent import Future
import httplib
from rest_api.api_client import ApiClient
from server.rest_api.models.robot_command import  as RobotCommand
from server.rest_api.models.robot_status import  as RobotStatus
from server.rest_api.models.drive_command import as DriveCommand
from server.rest_api.models.fire_command import as FireCommand
from server.game_model import Board, Robot

import time
from datetime import timedelta
from game_model import *
import json


class TurnAnswerFuture(Future):
    """A TurnHandler waiting a response. The response will be a RobotStatus.
    """

    # The token of the robot waiting for an answer.
    token = None

# A dictionay (map) between a robot token, and the corresponding RobotCommand
# Only allowed tokens from the game server are in this map.
# A map to None in case the robot has not yet sent the command.
next_robot_commands = {}

def add_next_robot_command(command):
    """Return True if the command was added.
    False if the command is not valid: the robot does not exists, or the robot sent two or more commands in the same turn.

    :param command: RobotCommand
    """
    if command.token in next_robot_commands:
        token = command.token.token
        if next_robot_commands[token] is None:
            next_robot_commands[token] = command
            return True
    return False

# A list of TurnAnswerFuture.
# Every TurnHandler registers itself in this list, for waiting a response.
queued_turn_handler_requests = []

# The current game
game_board = None
""" :var game_board: Board
"""

# The Swagger generated ApiClient
api_client = None

def process_game_turn():
    """Process game turn. Update the global state, and send answers to queued turn handlers.
    """
    global queued_turn_handler_requests
    global next_robot_commands
    global game_board

    # TODO no robot can send a command 2 times, because it does not known in advance the next-token, until it does not receive an answer
    # TODO leave to None the not received commands
    # TODO update a stats table with the not received commands for a given robot, using robot-name as key
    # TODO start answering to Futures (I must put in the future class the robot token)
    # TODO return
    # TODO see if it must also complete common answers for board-viewers, in case first favour answers to robot, then complete answers for board viewers that are on a separate queue

    #
    # Send commands to robots
    #

    new_next_robot_commands = {}

    # TODO essere sicuri di modificare il token quello anche della configurazione e non uno a caso

    from_old_to_new_tokens = {}
    for token, command in next_robot_commands.iteritems():
        """ :var command: RobotCommand
        """
        robot = game_board.get_robot_by_token(token)
        robot.set_next_command(command)
        new_token = robot.get_token()
        new_next_robot_commands[new_token] = None
        from_old_to_new_tokens[token] = new_token

    #
    # Advance the board.
    #

    game_board.simulate_for_a_turn()

    #
    # Return robot state to TurnHandlers
    #

    handlers_to_process = queued_turn_handler_requests[:]

    # Create a new server state, because after we start answering to Handlers, they can start making request again
    queued_turn_handler_requests = []
    next_robot_commands = new_next_robot_commands

    for turn_handler in handlers_to_process:
        """ :var turn_handler: TurnAnswerFuture
        """
        old_token = turn_handler.token()
        token = from_old_to_new_tokens[old_token]
        turn_handler.set_result(game_board.get_robot_by_token(token).get_status())


class TurnHandler(tornado.web.RequestHandler):
    """A couroutine receiving requests from remote-clients, and sending answers at each game turn.
    """

    @gen.coroutine
    def post(self):
        global queued_turn_handler_requests
        global api_client

        if self.request.headers["Content-Type"].startswith("application/json"):
            try:
                command = api_client.__deserialize(self.request.body, RobotCommand)
                if command is None:
                    self.send_error(tornado.web.HTTPError(httplib.BAD_REQUEST))
                else:
                    if add_next_robot_command(command):
                        handler_future = Future()
                        queued_turn_handler_requests.append(handler_future)
                        robot_status = yield handler_future

                        self.set_header("Content-Type", "application/json")
                        self.write(api_client.sanitize_for_serialization(robot_status))

                else:
                    self.send_error(tornado.web.HTTPError(httplib.BAD_REQUEST))
            except:
                self.send_error(tornado.web.HTTPError(httplib.BAD_REQUEST))
        else:
            self.send_error(tornado.web.HTTPError(httplib.BAD_REQUEST))

class CreateRobotHandler(tornado.web.RequestHandler):
    """Create a new robot, if the request is correct.
    """
    # TODO complete
    # TODO complete also documentation

class RemoveRobotHandler(tornado.web.RequestHandler):
    """Remove a robot from the game.
    """
    # TODO


# TODO support the handler for the returning of a board in debug mode
def make_app():
    return tornado.web.Application([
        (r"/robot-create", CreateRobotHandler),
        (r"/robot-remove", RemoveRobotHandler),
        (r"/robot-action", TurnHandler),
    ])

# TODO init game

# TODO use the port specified on the command line
if __name__ == "__main__":
    api_client = ApiClient()
    app = make_app()
    app.listen(8888)

    gameServer = PeriodicCallback(process_game_turn(), turnDeltaTime * 1000, tornado.ioloop.IOLoop.current())
    gameServer.start()
    tornado.ioloop.IOLoop.current().start()

'''

OLD CODE TO ADAPT

import threading
from client.netrobots_pb2 import *
import sys
import traceback

COMMAND_WAKE_UP = b"tick"
COMMAND_RESET_GAME = b"reset-game"
COMMAND_GET_BOARD = b"get-board"



    def process_create_robot_request(self, request, sender_address, client_socket):
        """
        Create a Robot. If the Robot does not respect the constraints it is returned dead,
        and with wellSpecifiedRobot = False

        :param sender_address: string
        :param request: CreateRobot
        :param client_socket:
        """

        extra = dict(
            max_hit_points=request.maxHitPoints,
            max_speed=request.maxSpeed,
            acceleration=request.acceleration,
            decelleration=request.decelleration,
            max_sterling_speed=request.maxSterlingSpeed,
            max_scan_distance=request.maxScanDistance,
            max_fire_distance=request.maxFireDistance,
            bullet_speed=request.bulletSpeed,
            bullet_damage=request.bulletDamage,
            reloading_time=request.reloadingTime
        )

        status = self._board.create_robot(request.name, extra, self._deltaTime, self._realTime)
        client_socket.send_multipart([sender_address, b'', status.SerializeToString()])

    def process_delete_robot_request(self, request, sender_address, client_socket):
        self._board.remove_robot_by_token(request.token)
        client_socket.send_multipart([sender_address, b'', b''])

'''