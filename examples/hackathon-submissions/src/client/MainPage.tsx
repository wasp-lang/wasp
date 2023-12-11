import React from 'react';
import betathonLogo from './betathonLogo.png';
import './Main.css';
import Nav from './components/Navbar';
import Form from './components/SubmissionForm';
import Projects from './components/Projects';

const MainPage = () => {
  return (
    <div>
      <Nav />
      <main>
        <img
          alt='betathon logo'
          src={betathonLogo}
          className=' mb-16 shadow-lg border-2 rounded-md border-yellow-500/25'
          width={600}
          height={600}
        />

        <Projects />
        <Form />
      </main>
    </div>
  );
};
export default MainPage;
