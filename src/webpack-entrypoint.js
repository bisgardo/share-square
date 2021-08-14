'use strict';

import { Elm } from './Main.elm';

const app = Elm.Main.init({
    node: document.getElementById('app'),
    flags: {
        environment: process.env.NODE_ENV, // populated with the 'mode' value by webpack 
    },
});
