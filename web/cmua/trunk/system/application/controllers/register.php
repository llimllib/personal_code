<?php

class Register extends Controller {
    function index($e='') {
        $this->load->model('RegisterModel');

        $this->load->view('addregistration', array('e' => $e, 
                                                   'session' => $this->db_session,
                                                   'title'   => 'CMUA 2008 Summer League Registration'));
    }

    function listall() {
        if (!$this->db_session->userdata('GROUP_Admin')) {
            $this->index();
            return;
        }

        $this->load->model('RegisterModel');
        $this->load->library('table');

        $tmpl = array (
                    'table_open'          => '<table border="0" cellpadding="4" cellspacing="0" width="2500">',
                    'row_start'           => '<tr style="background-color:lightgray">',
                    'row_alt_start'       => '<tr style="background-color:white">',
              );
        $this->table->set_template($tmpl); 
        
        $viewdata = array('reg_table' => $this->table->generate(
                                $this->db->query("SELECT * FROM register")));

        $this->load->view('reg_list', $viewdata);
    }

    function xxsecretxx() {
        $this->load->view('regsuccess');
    }

    function add() {
        $this->load->library('db_session');
        $this->load->model('RegisterModel');

        //save the user's data so we can fill the form if necessary
        foreach ($_POST as $key => $val)
        {
            $this->db_session->set_flashdata($key, $val);
        }

        //now validate the data. Real basic validation; let's not reject
        //anybody's app. Just make sure the required fields are filled
        $required = array('name', 'address', 'city', 'state', 'zip',
                          'gender', 'category', 'dob');
        foreach ($required as $r)
        {
            if (!$this->input->post($r) || strlen($this->input->post($r)) < 1)
                return $this->index("Failed to register: missing $r");
            if ($r == 'dob' && 
                !preg_match('/^[0-9]{2}\/[0-9]{2}\/[0-9]{2}$/', 
                            $this->input->post('dob')))
                return $this->index("Failed to register: invalid dob format");
        }

        if (!$this->input->post('waiver'))
            return $this->index("You need to agree to the waiver before completing your registration");

        if (!$this->input->post('league1') && !$this->input->post('league2'))
            return $this->index("Failed to register: pick a league");

        //fix the league data
        $league1 = $this->input->post('league1') ? 'm' : '';
        $league2 = $this->input->post('league2') ? 'w' : '';
        $league = $league1 . $league2;
                
        $this->RegisterModel->addregistration(
            $this->input->post('name'),
            $this->input->post('address'),
            $this->input->post('address2'),
            $this->input->post('city'),
            $this->input->post('state'),
            $this->input->post('zip'),
            $this->input->post('phone1'),
            $this->input->post('phone2'),
            $this->input->post('phone3'),
            $this->input->post('dob'),
            $this->input->post('gender'),
            $this->input->post('mailing_address'),
            $this->input->post('category'),
            '',
            $this->input->post('club_experience'),
            $this->input->post('baggage'),
            $this->input->post('baggage2'),
            $league,
            $this->input->post('exceptions')
        );

        $this->load->view('regsuccess', 
                          array('title' => '2008 CMUA Summer League Registration'));
    }
}

?>
