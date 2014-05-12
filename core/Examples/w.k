<code> nat <is> ['0 nat, '1 nat, '_];

nat 0 = '_;
nat 1 = '1'_;
nat 2 = '1'0'_;

/*
(nat -> nat) następnik <is> 
     wyrzuć_zera | odwróć | rec_następnik | odwróć ;
*/

(nat -> nat) wyrzuc_zera <is> [
     ('0 nat -> $.0) | wyrzuc_zera ,
     ('1 ... -> $) ];

(nat -> nat) odwroc <is> [
     ('_ -> '_),
     ('0 ... -> $.0) | odwroc | d0,     
     ('1 ... -> $.1) | odwroc | d1 ];

(nat -> nat) d0 <is> [
     ('_ -> '0'_),
     ('0 ... -> '0 [$.0 | d0]),
     ('1 ... -> '1 [$.1 | d0])];
     
(nat -> nat) d1 <is> [
     ('_ -> '1'_),
     ('0 ... -> '0 [$.0 | d1]),
     ('1 ... -> '1 [$.1 | d1])];
     

      
nat main = '0'0'1'1'1'0'_ | wyrzuc_zera | odwroc;

      