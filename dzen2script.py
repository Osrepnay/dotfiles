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
    gpu_dump = subprocess.run(["radeontop", "-d", "-", "-l", "1"], capture_output = True, text = True).stdout[:-1]
    gpu_usage = int(float(gpu_dump.split(", ")[2][4:-1]))
    memory_usage = int(psutil.virtual_memory().percent)
    curr_time = time.strftime("%a %b %d %I:%M:%S%p", time.localtime()).replace("AM", "pm").replace("PM", "pm")
    volume = subprocess.run(["pamixer", "--get-volume"], capture_output = True, text = True).stdout[:-1]
    statusbar = (f"CPU: {cpu_usage}%" +
        f"{posLeft(130)}GPU: {gpu_usage}%" +
        f"{posLeft(260)}Mem: {memory_usage}%" +
        f"{posRight(-450)}Volume: {volume}%" +
        f"{posRight(-275)}{curr_time}")
    print(statusbar, flush = True)
    time.sleep(1)
