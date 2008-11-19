<?php

class RegisterModel extends Model {
    function RegisterModel() {
        parent::Model();
    }

    function addregistration(
        $name,
        $address,
        $address2,
        $city,
        $state,
        $zip,
        $phone1,
        $phone2,
        $phone3,
        $dob,
        $gender,
        $mailing_address,
        $category,
        $rating,
        $club_experience,
        $baggage,
        $baggage2,
        $league,
        $exceptions)
    {
        #TODO if they sign up for both leagues, this should create
        #     two registrations?
        $data = array(
                    'name' =>            $name,
                    'address' =>         $address,
                    'address2' =>        $address2,
                    'city' =>            $city,
                    'state' =>           $state,
                    'zip' =>             $zip,
                    'phone1' =>          $phone1,
                    'phone2' =>          $phone2,
                    'phone3' =>          $phone3,  
                    'dob' =>             $dob,
                    'gender' =>          $gender,
                    'mailing_address' => $mailing_address,
                    'category' =>        $category,
                    'rating' =>          $rating,
                    'club_experience' => $club_experience,
                    'baggage' =>         $baggage,
                    'baggage2' =>        $baggage2,
                    'league' =>          $league,
                    'exceptions' =>      $exceptions,
                     );
        $this->db->insert('register', $data);
    }
}
?>
