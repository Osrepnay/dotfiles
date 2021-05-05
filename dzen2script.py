#!/usr/bin/python3
import psutil
import subprocess
import time

def posLeft(offset):
    return f"^p(_LEFT)^p({offset})"

def posCenter(offset):
    return f"^p(_CENTER)^p({offset})"

def posRight(offset):
    return f"^p(_RIGHT)^p({offset})"

while True:
    cpu_usage = int(psutil.cpu_percent())
    memory_usage = int(psutil.virtual_memory().percent)
    curr_time = time.strftime("%a %b %I:%M:%S%p", time.localtime()).replace("AM", "pm").replace("PM", "pm")
    volume = subprocess.run(["pamixer", "--get-volume"], capture_output = True).stdout.decode("UTF-8")[:-1]
    statusbar = (f"CPU: {cpu_usage}%" +
        f"{posLeft(130)}Mem: {memory_usage}%" +
        f"{posRight(-400)}Volume: {volume}%" +
        f"{posRight(-225)}{curr_time}")
    print(statusbar, flush = True)
    time.sleep(1)
