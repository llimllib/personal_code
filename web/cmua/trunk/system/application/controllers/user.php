<?php

class User extends Controller {
    function User()
    {
        parent::Controller();
    }

    function register() {
        if ($this->db_session->userdata('LOGGED_IN'))
            redirect('news', 'location');

        $this->load->view('registeruser');
    }

    function insertnew() {
        if ($this->db_session->userdata('LOGGED_IN'))
            redirect('news', 'location');
    }
}
