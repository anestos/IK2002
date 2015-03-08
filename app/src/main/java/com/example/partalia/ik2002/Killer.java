package com.example.partalia.ik2002;

public class Killer {
    private volatile Boolean running;
    private static Killer instance;

    public Killer(){
        running = true;

    }

    public Boolean getRunning() {
        return running;
    }

    public void setRunning(Boolean running) {
        this.running = running;
    }

    public static Killer getInstance(){
        if (instance == null){
            instance = new Killer();
        }
        return instance;

    }

}
