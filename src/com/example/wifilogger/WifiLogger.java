package com.example.wifilogger;

import android.os.Bundle;
import android.os.PowerManager;
import android.os.PowerManager.WakeLock;
import android.app.Activity;
import android.content.Context;
import android.view.Menu;
import android.view.View;
import android.view.inputmethod.InputMethodManager;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.TextView;
import android.widget.Toast;

import com.example.wifilogger.WifiLogToFile;

public class WifiLogger extends Activity {
	private boolean isRunning = false;   //Logs data
	private WifiLogToFile c = new WifiLogToFile();
	
    private PowerManager powerManager;
	private WakeLock wakeLock;
	
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_wifi_logger);
        
        powerManager=(PowerManager) getSystemService(Context.POWER_SERVICE);
        wakeLock=powerManager.newWakeLock(PowerManager.PARTIAL_WAKE_LOCK, "Wifi");
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        getMenuInflater().inflate(R.menu.activity_wifi_logger, menu);
        return true;
    }
    
    public void startStopLogging(View view) {
    	/* Handle the button logging (Start/Stop) */
    	Button editText = (Button) findViewById(R.id.StartStopLogging);
    	TextView statusText = (TextView) findViewById(R.id.status);
    	TextView room_number = (TextView) findViewById(R.id.roomNumber);
    	CheckBox filterEduRoam = (CheckBox) findViewById(R.id.filterEduRoamP);
    	
    	if(editText.getText().equals("Start Logging")) {
    		c.prep(String.valueOf(room_number.getText()), this, filterEduRoam.isChecked());
    		new Thread(c).start();
    		isRunning = true;
    		Toast.makeText(WifiLogger.this,"Beginning logging on room "+room_number.getText(), Toast.LENGTH_SHORT).show();
    		statusText.setText("Logging Started, file: " + c.getFile());
    		editText.setText("Stop Logging");
    	} else if (editText.getText().equals("Stop Logging")) {
    		isRunning = false;
    		c.stop();
    		Toast.makeText(WifiLogger.this, "Logging Stopped...", Toast.LENGTH_SHORT).show();
    		statusText.setText("Logging Stopped");
    		editText.setText("Start Logging");
    	}
    	InputMethodManager imm = (InputMethodManager)getSystemService(
    		      Context.INPUT_METHOD_SERVICE);
    		imm.hideSoftInputFromWindow(room_number.getWindowToken(), 0);
    }
    
}
