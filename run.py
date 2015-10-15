# Run a test server.

from flask import Flask
from app import app
import sys

app.config['HOST_IP']=sys.argv[1]
app.config['ZMQ_PORT']=int(sys.argv[3])
app.config['SIMULATION_SPEED']=int(sys.argv[4])
app.config['NETWORK_SPEED']=int(sys.argv[5])

app.run(host=sys.argv[1], port=int(sys.argv[2]), debug = False)
