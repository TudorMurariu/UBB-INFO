package utils.events;

import domain.Drug;

public class MedsManagementEvent implements Event{
    private MedsManagementType type;
    private Drug data,oldData;

    public MedsManagementEvent(MedsManagementType type, Drug data){
        this.type = type;
        this.data = data;
    }

    public MedsManagementEvent(MedsManagementType type, Drug data, Drug oldData){
        this.type = type;
        this.data = data;
        this.oldData = oldData;
    }

    public MedsManagementType getType() {
        return type;
    }

    public Drug getData() {
        return data;
    }

    public Drug getOldData() {
        return oldData;
    }
}
