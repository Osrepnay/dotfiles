#!/usr/bin/python3
import time

while True:
    curr_time = time.strftime("%a %b %d %I:%M:%S%p", time.localtime()).replace("AM", "pm").replace("PM", "pm")
    statusbar = f"^p(_LEFT)^p(_RIGHT)^p(-275){curr_time}"
    print(statusbar, flush = True)
    time.sleep(0.9)
