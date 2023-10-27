import React, { memo } from 'react';
import { IonItem, IonLabel, IonCheckbox, IonDatetime } from '@ionic/react';
import { ItemProps } from './ItemProps';

interface ItemPropsExt extends ItemProps {
  onEdit: (id?: string) => void;
}

const Item: React.FC<ItemPropsExt> = ({ id, name, description, date, closed, onEdit }) => {
  return (
    <IonItem onClick={() => onEdit(id)}>
      <IonLabel>{name}</IonLabel>
      <IonLabel>{description}</IonLabel>
      {/* <IonDatetime value={date}></IonDatetime> */}
      <IonCheckbox checked={!closed}></IonCheckbox>
    </IonItem>
  );
};

export default memo(Item);
