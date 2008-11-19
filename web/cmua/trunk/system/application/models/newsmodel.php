<?php

class NewsModel extends Model {
    function NewsModel() {
        parent::Model();
    }

    function get_most_recent($n) {
        $query = $this->db->orderby('newsdt', 'desc');
        $query = $this->db->get('News', $n);
        if ($query->num_rows() > 0)
            return $query->result();
        //TODO: log this as an error
        return false;
    }

    function get($id) {
        if (!preg_match("/^[0-9]*$/", $id)) return array('title' => 'regex prob');
        $query = $this->db->query("SELECT * FROM News WHERE id=$id");
        $res = $query->result();
        return $res[0];
    }

    function updatenews($id, $title, $body, $author) {
        $data = array(
                    'title' => $title,
                    'body' => $body,
                    'author' => $author,
                );
        $this->db->update('News', $data, "id=$id");
    }

    function addnews($title, $body, $author) {
        $data = array(
                    'title' => $title,
                    'body' => $body,
                    'author' => $author,
                );
        $this->db->insert('News', $data);
    }

    function del($id) {
        if (!preg_match("/^[0-9]*$/", $id)) return FALSE;
        $this->db->query("DELETE FROM News WHERE id=$id");
        return TRUE;
    }
}

?>
