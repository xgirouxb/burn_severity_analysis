import ee
import time

def monitor_gee_task(task, check_interval=120):
    """
    Monitors a GEE task every `check_interval` seconds until it's completed or failed.
    
    Parameters:
        task (ee.batch.Task): The GEE task object to monitor.
        check_interval (int): Time to wait (in seconds) between status checks.
        
    Returns:
        str: Success message if task is complete, or error message if failed.
    """
    print(f"Monitoring task {task.id}...")

    while True:
        status = task.status()
        state = status['state']
        print(f"Task status: {state}")
        
        if state == 'COMPLETED':
            print("Task completed successfully.")
            return "Task completed successfully."
        
        elif state == 'FAILED':
            error_message = status.get('error_message', 'No error message provided.')
            print(f"Task failed. Error: {error_message}")
            return f"Task failed: {error_message}"
        
        elif state in ['CANCELLED', 'CANCELED']:
            print("Task was cancelled.")
            return "Task was cancelled."
        
        # Wait and check again
        time.sleep(check_interval)
