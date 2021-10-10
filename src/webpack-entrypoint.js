'use strict';

// Import CSS directly into 'app.js'.
// It will be imported into the HTML from there (presumably through the webpack CSS plugins).
import 'bootstrap/dist/css/bootstrap.min.css';
import { Modal } from 'bootstrap';

// Import all the Elm code.
import { Elm } from './Main.elm';

const app = Elm.Main.init({
    node: document.getElementById('app'),
    flags: {
        environment: process.env.NODE_ENV, // populated with the 'mode' value by webpack 
    },
});

window.addEventListener('hidden.bs.modal', event => {
    const modalId = event.target.id;
    app.ports.modalClosed.send(modalId);
});
app.ports.closeModal.subscribe(id => {
	const element = document.getElementById(id);
    Modal.getInstance(element).hide();
});
