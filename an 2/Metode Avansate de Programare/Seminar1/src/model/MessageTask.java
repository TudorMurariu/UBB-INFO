package model;

import utils.Constants;

import java.time.LocalDateTime;

public class MessageTask extends Task {
    private String mesaj;
    private String from;
    private String to;

    private LocalDateTime date;

    @Override
    public void execute() {
        System.out.println(toString());
    }

    @Override
    public String toString() {
        return super.toString() +
                " " + mesaj +
                " " + from +
                " " + to +
                " " + date.format(Constants.DATE_TIME_FORMATTER);
    }

    public MessageTask(String taskId, String description, String mesaj, String from, String to, LocalDateTime date) {
        super(taskId, description);
        this.mesaj = mesaj;
        this.from = from;
        this.to = to;
        this.date = date;
    }

    public String getMesaj() {
        return mesaj;
    }

    public void setMesaj(String mesaj) {
        this.mesaj = mesaj;
    }

    public String getFrom() {
        return from;
    }

    public void setFrom(String from) {
        this.from = from;
    }

    public String getTo() {
        return to;
    }

    public void setTo(String to) {
        this.to = to;
    }

    public LocalDateTime getDate() {
        return date;
    }

    public void setDate(LocalDateTime date) {
        this.date = date;
    }
}
