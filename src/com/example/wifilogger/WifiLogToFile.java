package com.example.wifilogger;

import android.content.Context;
import android.net.wifi.ScanResult;
import android.net.wifi.WifiConfiguration;
import android.net.wifi.WifiInfo;
import android.net.wifi.WifiManager;
import android.net.wifi.WifiManager.WifiLock;
import android.os.Environment;
import android.widget.Toast;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.BlockingQueue;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class WifiLogToFile implements Runnable {

	private FileWriter fw = null;
	private boolean letRun = false;
	private boolean finishedp = false;
	private String myFile = "";
	
	private WifiManager m_wifiManager;
	private WifiInfo m_wifiInfo;
	private WifiLock m_wifiLock;
	private List<WifiConfiguration> m_wifiConfigList;
	private List<ScanResult> m_wifiList;
	private long m_timeStamp;
	private List<ScanResult> m_wholeList;
	private boolean filterEduRoamP;
	
	public void run() {
		while(letRun) {	
			getScanningResults();
			try {
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}
	
	private void getScanningResults () {
		if(m_wifiManager.startScan())
		{
			m_wholeList=m_wifiManager.getScanResults();
			if(m_wholeList!=null) 
			{	for(int i=0;i<m_wholeList.size();i++)
				{
					if (filterEduRoamP && !m_wholeList.get(i).SSID.equals("eduroam"))
						continue;
					writeSSIDandStrength(m_wholeList.get(i).SSID,
							m_wholeList.get(i).level);
			}
		}
		m_wifiConfigList=m_wifiManager.getConfiguredNetworks();	
		}
	}
	
	private void writeSSIDandStrength(String SSID, int Level) {
		writeToFile(SSID + "," + String.valueOf(Level) + "\r\n");
	}
	 private void writeToFile(String stringToWrite) {
	    	try {
				fw.write(stringToWrite);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
	 }

	 public void stop() {
		letRun = false;
		if(fw != null) {
			closeFile();
			fw = null;
		}
		
		finishedp = true;
	}
	 
	 public boolean closeFile() {
		 try {
	    	fw.flush();
			fw.close();
			return true;
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return false;
		}
	 }
	 
     public String getDateFormat() {
    	Date dateNow = new Date ();
		SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy.MM.dd_HH.mm.ss");
		StringBuilder myDate = new StringBuilder( dateFormat.format( dateNow ) );
		return myDate.toString();
    }
	    
 	public void prep(String room_number, Context context, Boolean filterEduRoam) {
 		filterEduRoamP = filterEduRoam;
		finishedp = false;
		m_wifiManager=(WifiManager)context.getSystemService(Context.WIFI_SERVICE);
		m_wifiList=new ArrayList<ScanResult>();
		openOutputFile(room_number);
		writeToFile("Mac_Addr,Strength\r\n");
		letRun = true;
	}
 	
 	public String getFile() {
 		if(fw != null) {
 			return myFile;
 		} else {
 			return "";
 		}
 	}
     private boolean openOutputFile(String room_number) {
    	try {
    		myFile = Environment.getExternalStorageDirectory().toString() + File.separator + 
    				room_number + "." + getDateFormat() + ".csv";
    		File logOutputFile = new File(myFile);
	   		if(!logOutputFile.exists()) {
	   			logOutputFile.createNewFile();
	   		}
		
	   		fw = new FileWriter(logOutputFile);
	   		return true;
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return false;
		}
	}
}
