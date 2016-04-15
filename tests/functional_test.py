'''
    Copyright 2015, 2016 Roberto Gambuzzi <gbinside@gmail.com>
    Copyright 2015, 2016 Massimo Zaniboni <massimo.zaniboni@docmelody.com>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License,
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
'''

"""Test the HTTP server application.
"""

import json
import tornado.testing
from server.rest_api import *
import server.game_server

class TestNetRobotsApp(tornado.testing.AsyncHTTPTestCase):

    board_x_size = 1000.0
    board_y_size = 1000.0
    network_latency = 0.100
    game_tick = 0.25

    _api_client = None   # type: DefaultApi
    _api_client_conf = None # type: ApiClient

    _json_headers = None

    def init_game_board(self):
        server.game_server.init_game_model(self.board_x_size, self.board_y_size, self.network_latency, self.game_tick)

    def start_game_server(self):
        server.game_server.create_game_server(self.board_x_size, self.board_y_size, self.network_latency, self.game_tick)

    def process_game_turn(self):
        server.game_server.process_game_turn()

    def get_app(self):
        self._json_headers = {}
        self._json_headers['Content-Type'] = 'application/json'

        self.init_game_board()
        app = server.game_server.make_app()
        self._api_client = ApiClient(self.get_url('/'))
        return app

    def default_robot_configuration(self, robotName):
        """
        :type robotName: str
        :rtype: RobotConfiguration
        """
        r = RobotConfiguration()

        r.name = robotName
        r.max_hit_points = 100.0
        r.max_speed = 27.0
        r.acceleration = 9.0  # m/s^2
        r.decelleration = -5.0  # m/s^2
        r.max_sterling_speed = 13.0
        r.max_scan_distance = 700.0
        r.max_fire_distance = 700.0
        r.bullet_speed = 500.0  # m/s
        r.bullet_damage = 10.0
        r.cannon_reloading_time = 1.0
        # NOTE: strenght is optimized from the server

        return r

    def test_empty_board(self):
        self.init_game_board()
        rv = self.fetch('/board-events')
        jd = json.loads(rv.body)
        self.assertEqual(jd['robots'], [])
        self.assertEqual(jd['size'], [1000.0, 1000.0])

    def test_new_robot(self):
        self.init_game_board()
        rc1 = self.default_robot_configuration("GUNDAM")
        jd = json.dumps(self._api_client.sanitize_for_serialization(rc1))
        rc2 = self.fetch('/robot-create', method="POST", body=jd, headers=self._json_headers)
        rc3 = json.loads(rc2.body)

        self.assertEqual(rc3['name'], "GUNDAM")
        self.assertEqual(rc3['isDead'], False)

    def test_drive(self):
        self.start_game_server()

        token = None

        # Create Robot

        rc1 = self.default_robot_configuration("GUNDAM")
        jd = json.dumps(self._api_client.sanitize_for_serialization(rc1))
        rc2 = self.fetch('/robot-create', method="POST", body=jd, headers=self._json_headers)
        rc3 = json.loads(rc2.body)
        token = rc3['token']

        self.assertEqual(rc3['name'], "GUNDAM")
        self.assertEqual(rc3['isDead'], False)

        # Command

        drive = DriveCommand()
        drive.direction = 90.0
        drive.speed = 50.0

        cmd = RobotCommand()
        cmd.drive = drive
        cmd.token = token

        jd = json.dumps(self._api_client.sanitize_for_serialization(cmd))
        rc2 = self.fetch('/robot-action', method="POST", body=jd, headers=self._json_headers)
        rc3 = json.loads(rc2.body)
        token = rc3['token']

        self.assertEqual(rc3['isDead'], False)
        self.assertEqual(float(rc3['direction']), 90.0)

        # Board View

        rv = self.fetch('/board-events')
        jd = json.loads(rv.body)
        self.assertEqual(jd['robots'][0]['name'], "GUNDAM")
        self.assertEqual(jd['size'], [1000.0, 1000.0])

        # Command

        drive = DriveCommand()
        drive.direction = 60.0
        drive.speed = 50.0

        cmd = RobotCommand()
        cmd.drive = drive
        cmd.token = token

        jd = json.dumps(self._api_client.sanitize_for_serialization(cmd))
        rc2 = self.fetch('/robot-action', method="POST", body=jd, headers=self._json_headers)
        rc3 = json.loads(rc2.body)
        token = rc3['token']

        self.assertEqual(rc3['isDead'], False)
        self.assertEqual(float(rc3['direction']), 60.0)

        # Command

        drive = DriveCommand()
        drive.direction = 70.0
        drive.speed = 50.0

        cmd = RobotCommand()
        cmd.drive = drive
        cmd.token = token

        jd = json.dumps(self._api_client.sanitize_for_serialization(cmd))
        rc2 = self.fetch('/robot-action', method="POST", body=jd, headers=self._json_headers)
        rc3 = json.loads(rc2.body)
        token = rc3['token']

        self.assertEqual(rc3['isDead'], False)
        self.assertEqual(float(rc3['direction']), 70.0)


if __name__ == '__main__':
    tornado.testing.main()
