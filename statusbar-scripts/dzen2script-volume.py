#!/usr/bin/python3
import os
import socket
import subprocess
import time

if os.path.exists("/tmp/volume-socket"):
    os.remove("/tmp/volume-socket")

volume = subprocess.run(["pamixer", "--get-volume"], capture_output = True, text = True).stdout[:-1]
statusbar = f"^p(_RIGHT)^p(-230)^i(/home/archbox/statusbar-scripts/icons/sound.xbm) {volume}%"
print(statusbar, flush = True)

server = socket.socket(socket.AF_UNIX, socket.SOCK_DGRAM)
server.bind("/tmp/volume-socket")
while True:
    datagram = server.recv(8)
    if not datagram:
        break
    else:
        volume = int.from_bytes(datagram, 'little')
        statusbar = f"^p(_RIGHT)^p(-230)^i(/home/archbox/statusbar-scripts/icons/sound.xbm) {volume}%"
        print(statusbar, flush = True)
