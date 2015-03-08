package com.ik2002.project.ik2002;

public class Killer {
    private volatile Boolean running;
    private volatile Boolean chatting;
    private static Killer instance;

    public Killer(){
        running = true;
        chatting = true;

    }

    public Boolean getRunning() {
        return running;
    }
    public Boolean getChatting() {
        return chatting;
    }

    public void setRunning(Boolean running) {
        this.running = running;
    }
    public void setChatting(Boolean chatting) {
        this.chatting = chatting;
    }

    public static Killer getInstance(){
        if (instance == null){
            instance = new Killer();
        }
        return instance;

    }

}
