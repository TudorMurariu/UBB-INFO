import { IonButton, IonContent, IonHeader, IonPage, IonTitle, IonToolbar } from '@ionic/react';
import ExploreContainer from '../components/ExploreContainer';
import { Link } from 'react-router-dom';

const Test: React.FC = () => {
  return (
    <IonPage>
        <h1>Aaaaaa</h1>
      <IonHeader>
        <IonToolbar>
          <IonTitle>AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA</IonTitle>
        </IonToolbar>
      </IonHeader>
      <IonContent fullscreen>
        <IonHeader collapse="condense">
          <IonToolbar>
            <IonTitle size="large">AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA</IonTitle>
          </IonToolbar>
        </IonHeader>
        <ExploreContainer />
        <Link to="/home" style={{ color: 'white' }}><IonButton>Back to home</IonButton></Link>
      </IonContent>
    </IonPage>
  );
};

export default Test;
