<?php

class AuthModel extends Model {
    function AuthModel() {
        parent::Model();
    }

    function isValidUser($username, $password) {
        $this->db->where('user_name', $username);
        $this->db->where('password', $password);
        $query = $this->db->get('user');
        if ($query->num_rows()) {
            return $query->row();
        }
        return false;
    }
}
