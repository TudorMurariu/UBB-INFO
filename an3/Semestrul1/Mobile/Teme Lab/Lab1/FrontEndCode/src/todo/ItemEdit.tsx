import React, { useCallback, useContext, useEffect, useState } from 'react';
import {
  IonButton,
  IonButtons,
  IonContent,
  IonHeader,
  IonInput,
  IonLoading,
  IonPage,
  IonTitle,
  IonToolbar,
  IonCheckbox 
} from '@ionic/react';
import { getLogger } from '../core';
import { ItemContext } from './ItemProvider';
import { RouteComponentProps } from 'react-router';
import { ItemProps } from './ItemProps';

const log = getLogger('ItemEdit');

interface ItemEditProps extends RouteComponentProps<{
  id?: string;
}> {}

const ItemEdit: React.FC<ItemEditProps> = ({ history, match }) => {

  const { items, saving, savingError, saveItem } = useContext(ItemContext);
  const [name, setName] = useState('');
  const [description, setDescription] = useState('');
  const [date, setDate] = useState(new Date());
  const [closed, setClosed] = useState(false);
  const [item, setItem] = useState<ItemProps>();

  useEffect(() => {
    log('useEffect');
    const routeId = match.params.id || '';
    const item = items?.find(it => it.id === routeId);

    setItem(item);
    if (item) {
      setName(item.name)
      setDescription(item.description)
      setDate(item.date)
      setClosed(item.closed)
    }
  }, [match.params.id, items]);

  const handleSave = useCallback(() => {
    const editedItem = item ? { ...item, name, description, date, closed } : { name, description, date, closed  };
    saveItem && saveItem(editedItem).then(() => history.goBack());
  }, [item, saveItem, name, description, closed, history]);

  const handleCancel = useCallback(() => {
    history.goBack();
  }, [item, history]);

  log('render');
  return (
    <IonPage>
      <IonHeader>
        <IonToolbar>
          <IonTitle style={{ textAlign: 'center' }}>Edit Page</IonTitle>
        </IonToolbar>
      </IonHeader>

      <IonContent>
        <IonInput placeholder='name' value={name} onIonChange={e => setName(e.detail.value || '')} />
        <IonInput placeholder='description' value={description} onIonChange={e => setDescription(e.detail.value || '')} />
        <IonCheckbox checked={!closed} onIonChange={e => setClosed(!e.detail.checked)}> Is it open: </IonCheckbox>
        <IonLoading isOpen={saving} />
        
        {savingError && (
          <div>{savingError.message || 'Failed to save item'}</div>
        )}
        <IonToolbar>
          <IonButtons slot="start">
              <IonButton onClick={handleCancel}>
                Cancel
              </IonButton>
          </IonButtons>
          <IonButtons slot="end">
              <IonButton onClick={handleSave}>
                Save
              </IonButton>
          </IonButtons>
        </IonToolbar>
      </IonContent>
    </IonPage>
  );
};

export default ItemEdit;
