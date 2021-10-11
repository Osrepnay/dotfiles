#!/usr/bin/python3
import psutil
import subprocess
import time

while True:
    cpu_usage = int(psutil.cpu_percent())
    gpu_dump = subprocess.run(["radeontop", "-d", "-", "-l", "1"], capture_output = True, text = True).stdout[:-1]
    gpu_usage = int(float(gpu_dump.split(", ")[2][4:-1]))
    memory_usage = int(psutil.virtual_memory().percent)
    curr_time = time.strftime("%a %b %d %I:%M:%S%p", time.localtime()).replace("AM", "pm").replace("PM", "pm")
    volume = subprocess.run(["pamixer", "--get-volume"], capture_output = True, text = True).stdout[:-1]
    # needs a ^p(_RIGHT) after symbolic arg for some reason
    statusbar = (f"^p(_LEFT)^i(/home/archbox/statusbar-scripts/icons/cpu.xbm) {cpu_usage}%^p(_RIGHT)" +
        f"^p(_LEFT)^p(130)^i(/home/archbox/statusbar-scripts/icons/gpu.xbm) {gpu_usage}%^p(_RIGHT)" +
        f"^p(_LEFT)^p(260)^i(/home/archbox/statusbar-scripts/icons/ram.xbm) {memory_usage}%^p(_RIGHT)")
    print(statusbar, flush = True)
    time.sleep(1)
