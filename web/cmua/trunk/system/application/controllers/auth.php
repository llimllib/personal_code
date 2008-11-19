<?php

class Auth extends Controller {
    function Auth()
    {
        parent::Controller();
    }

    function login() {

        //TODO: understand how they do the validation stuffs
        if ($this->db_session->userdata('LOGGED_IN'))
        {
            redirect('/news', 'location');
        } else {
            $this->load->model('AuthModel');

            $username = $this->input->post('username');
            $password = $this->input->post('password');

            if ($username && $password) {
                $password = sha1($password);
                $result_row = $this->AuthModel->isValidUser($username, $password);
                if ($result_row) {
                    $this->db_session->set_userdata('LOGGED_IN', 
                                                    $result_row->user_name);
                    $this->_getperms($result_row->id);
                    redirect('news', 'location');
                }
                else
                    redirect('news', 'location');
            } else {
                redirect('news', 'location');
            }
        }
    }

    function _getperms($user_id) {
        //$user_id comes from a db query, and so doesn't need escaping
        $sql = <<<SQL
            SELECT sr.name
            FROM user u, security_role sr, security_role_user sru
            WHERE u.id = $user_id
            AND u.id = sru.user_id
            AND sr.id = sru.security_role_id 
SQL;

        $query = $this->db->query($sql);

        foreach ($query->result() as $row)
            $this->db_session->set_userdata("GROUP_{$row->name}", 'true');
    }

    function logout() {
        $this->db_session->sess_destroy();
        redirect('news', 'location');
    }

    function index($username='') {
        $this->load->model('AuthModel');

        $data['title'] = "Log In";
        $data['username'] = $username;

        $this->load->view('login', $data);
    }
}
