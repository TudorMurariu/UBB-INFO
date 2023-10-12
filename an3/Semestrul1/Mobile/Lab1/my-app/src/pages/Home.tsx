import { IonButton, IonContent, IonHeader, IonTitle, IonToolbar } from '@ionic/react';
import './Home.css';
import { Link } from 'react-router-dom';

const Home: React.FC = () => {
  return (
    <>
    <IonHeader>
      <IonToolbar>
        <IonTitle>Test</IonTitle>
      </IonToolbar>
    </IonHeader>
    <IonContent className="ion-padding">
      <h1>Content</h1>
        <Link to="/test" style={{ color: 'white' }}><IonButton>Go to Test Page</IonButton></Link>
    </IonContent>
  </>
  );
};

export default Home;
