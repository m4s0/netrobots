NETROBOT
========

This is a fork of [https://github.com/gbinside/netrobots], using Tornado Python Web Server, and a better REST API.

Freely inspired/based on P-ROBOTS [http://corewar.co.uk/probots/p-robo4.txt]

Up to date it is in alpha/development state. See `DEV.org` file for more info.

REST API
========

The API is described in an human readable Swagger format in `doc/rest_api.yaml` file.

Code for different programming languages, is derived from this file, using the tool `external_tools/swagger-codegen-cli-2.1.6.jar`.

In the `robot_examples` directory there are demo applications for different programming languages.

Up to date there are real tested robots only for Python.
For the other programming languages there is only the Swagger generated skeleton,
that can be used for acessing the server.

Getting Started
===============

TODO adapt these notes

Package Requirements
--------------------

On Debian/Ubuntu

  sudo aptitude install python-flask protbuf-compiler python-protobuf libzmq3 libzmq3-dev python-zmq

Starting
--------

On main directory

  python run.py [ip-address] [web-port] [zmq-port] [simulation-passes-by-second] [network-commands-by-second]

Suggest default configurations are:

  python run.py 0.0.0.0 8098 1234 4 2

With these settings:
* all the IP ports of the host can be used for starting web and ZMQ connessions
* http connesion start on port 8098
* zmq connessions start on port 1234
* every second of simulated time, there can be until 4 commands issued from any robot. So a robot can perform an action every 1/4 of simulated second
* every second of real time, the commands from clients are checked 2 times. Every command is related to 1/4 of simulated time, so the ratio between real time and simulated time is 1/2 in this case
* the ratio is used for allowing robots on small network to issue commands, slowing down the simulated world

Open the browser at

  http://[ip-address]:[web-port]/

Launch some demo robot, specifying the connection params:

  cd example/python
  python rabbit.py [ip-address] [zmq-port]i
  python sniper.py [ip-address] [zmq-port]

For example if they are on the same server

  python rabbit.py 127.0.0.1 1234
  python sniper.py 127.0.0.1 1234

You can launch a demo robot more than one time, if you want populate the board.

Robots Coding Instructions
==========================

Robots can be written using any programming language, because they communicate with the server using REST API.

Study `doc/rest_api.yaml` for a description of the API.

The code for many clients is generated from swagger, into `clients` directory. See also `external_tools` directory for the code generating the API.

Development
===========

See file `DEV.org`.  

LICENSE
=======

GPLv3+

