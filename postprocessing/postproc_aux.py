import subprocess
import datetime

def cdo_launch(cdo_command, log_handle=None):
    """ @brief Wrapper to execute CDO commands
        @param cdo_command Full path to the cdo command execute
        @param log_handle Handle wo which std_out/err is written
               Default 'None' indicates no logging

        This wrapper will take an cdo commandline, executes it and writes
        std_out/std_err to a log handle. 
    """
    std_outerr = None
    if (log_handle != None):
        log_handle.write("%%%%%%%%%%%%%%%%%%%%%%\n")
        log_handle.write("% Command started: " +  str(datetime.datetime.now()) + "\n")
        log_handle.write("%%%%%%%%%%%%%%%%%%%%%%\n")
        log_handle.write(cdo_command + "\n")
        run_application = subprocess.Popen(cdo_command, shell=True, stdout=subprocess.PIPE)
        run_application.wait()
        std_outerr = run_application.communicate()[0]
        log_handle.write("%%%%%%%%%%%%%%%%%%%%%%\n")
        log_handle.write("% Command finished: " +  str(datetime.datetime.now()) + "\n")
        log_handle.write("%%%%%%%%%%%%%%%%%%%%%%\n")
        log_handle.write(std_outerr)

    return std_outerr