<?php

class News extends Controller {
    function index() {
        $this->load->model('NewsModel');

        #TODO: load this number from a config file
        $data['news'] = $this->NewsModel->get_most_recent(10);
        $data['title'] = 'CMUA | Central Maryland Ultimate Association';

        $data = array_merge($this->db_session->userdata(), $data);

        $this->load->view('news', $data);
    }

    function edit()
    {
        if (!$this->db_session->userdata('GROUP_Admin')) {
            $this->index();
            return;
        }

        $this->load->model('NewsModel');
        $article = $this->NewsModel->get($this->uri->segment(3));
        $this->load->view('addnews', $article);
    }

    function del()
    {
        if (!$this->db_session->userdata('GROUP_Admin')) {
            $this->index();
            return;
        }

        $this->load->model('NewsModel');
        $this->NewsModel->del($this->uri->segment(3));
        $this->index();
    }

    function add_() {
        if (!$this->db_session->userdata('GROUP_Admin')) {
            $this->index();
            return;
        }

        $this->load->view('addnews');
    }

    function insertnew() {
        if (!$this->db_session->userdata('GROUP_Admin'))
            redirect('auth/login', 'location');

        $this->load->model('NewsModel');

        $title = $this->input->post('title');
        $body = $this->input->post('body');
        $id = $this->input->post('id');
        $author = $this->db_session->userdata('LOGGED_IN');

        if ($title && $body && $author)
            if ($id)
                $this->NewsModel->updatenews($id, $title, $body, $author);
            else
                $this->NewsModel->addnews($title, $body, $author);
        else
            $this->add();

        redirect('news', 'location');
    }
}
?>
