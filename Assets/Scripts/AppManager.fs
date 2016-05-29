namespace wordrocketfsharpy
open UnityEngine
open UnityEngine.UI
open System.Collections.Generic
open System.Linq

module tools=
    let debug = fun x-> Debug.Log(x)
    let add_listener (_button:Button)  _listener = _button.onClick.AddListener(Events.UnityAction _listener)
    let to_string (x:int) = x.ToString()
    let random (a:int) (b:int) = UnityEngine.Random.Range(a,b)
    let parse_int (a:string) = System.Int32.Parse(a)
    let error (s:string) = Debug.LogError(s)
    let load_txt_asset(path:string) = Resources.Load(path) :?> TextAsset
    let mutable_list_to_list = Microsoft.FSharp.Collections.List.ofSeq


type Word= { id:int; lang1:string; lang2:string}


module words=

    let parse_line (str:string) = 
        str.Split(';') 
        |> fun values->
        match values.Length with
            |3->
                (values.[0],values.[1],values.[2]) 
                |> fun (a,b,c)-> (tools.parse_int a,b,c)
                |> fun (a,b,c)-> {id=a; lang1=b;lang2=c} |> Some
            |_->
                tools.error "couldnt parse line"
                (None:Word option)

    let word_to_csv_file_line (w:Word) = 
        (w.id.ToString(),w.lang1, w.lang2)
        |>fun (a,b,c)->a+";"+b+";"+c
    let parse_csv_to_word_list (loaded_file:string) = 
        loaded_file.Split('\n')
        |> Array.map parse_line
        |> fun arr->arr.Where(fun x->x.IsSome).Select(fun x->x.Value).ToList()

    let all_words_path="korean"
    let reviewing_path="korean_reviewing"
    let deleted_words="korean_deleted"
    let all= 
        tools.load_txt_asset all_words_path
        |> fun ta->ta.text
        |> parse_csv_to_word_list
        |> fun word_list->
            word_list.Count |> fun x->"new words list lenght: "+x.ToString() |> tools.debug
            word_list

    let load_words_from_path (path:string) (l:List<Word>)=
        System.IO.File.Exists(path)
        |> fun _exist -> 
            match _exist with
            | false -> tools.debug <| "no file in path "+path
            | true -> 
                System.IO.File.ReadAllText(path) 
                |> parse_csv_to_word_list 
                |> l.AddRange
    let load_reviewing_list = load_words_from_path reviewing_path
    let load_deleted_list = load_words_from_path deleted_words

    
    let not_in_list (l:List<Word>) (w1:Word)= l.Any(fun w2->w2.id=w1.id) |> not
    let load_new_words (new_words:List<Word>) (reviewing:List<Word>) (deleted:List<Word>)=
        all
        |> tools.mutable_list_to_list
        |> List.filter (not_in_list deleted)
        |> List.filter (not_in_list reviewing)
        |> List.iter (fun w->new_words.Add(w))

    let save_to_file (path:string) (some_text:string) = System.IO.File.WriteAllText(path,some_text)

    let save_words_list (l:List<Word>) (file_path:string)=
        l
        |> tools.mutable_list_to_list
        |> List.map word_to_csv_file_line
        |> List.fold (+) "\n"
        |> fun txt->save_to_file file_path txt

    let rec get_random_word (some_list: Word list)=
        match some_list.Length with
            |0->None
            |_->tools.random 0 some_list.Length |> fun i->some_list.[i]|> Some

    let delete_file_at_path (path:string)=
        System.IO.File.Exists(path)
        |> fun exst->
           match exst with
            |false -> tools.debug "no file to delete"
            |true->
                System.IO.File.Delete(path)
                "deleted file at path"+path
                |>tools.debug
             
    let reset()=
        delete_file_at_path deleted_words
        delete_file_at_path reviewing_path


type AppManager() = 
    inherit MonoBehaviour()

    [<SerializeField>]
    let mutable word_text1=null:Text
    [<SerializeField>]
    let mutable word_text2=null:Text
    [<SerializeField>]
    let mutable newword_button=null:Button
    [<SerializeField>]
    let mutable review_button=null:Button
    [<SerializeField>]
    let mutable learned_button=null:Button
    [<SerializeField>]
    let mutable reset_button=null:Button

    let mutable (word_now:Word option) = None

    member this.Start() = 

        let words_deleted = new List<Word>()
        let words_reviewing = new List<Word>()
        let words_new = new List<Word>()

        words.load_reviewing_list words_reviewing
        words.load_deleted_list words_deleted
        words.load_new_words words_new words_reviewing words_deleted |> ignore

        let no_words_to_review()= 
            word_text1.text<-"no words to review"
            word_text2.text<-""
        let no_new_words()=
            word_text1.text<-"no new words"
            word_text2.text<-""

        let word_to_text_component (w:Word) =
            word_text1.text <- w.lang1
            word_text2.text <- w.lang2

        let new_word() = 
            words_new 
            |> tools.mutable_list_to_list
            |> words.get_random_word
            |> fun random_w->
                match random_w with
                    |None-> no_new_words()
                    |Some w->
                        word_to_text_component w
                        words_reviewing.Add(w)
                        word_now <- Some w
                        words_new.RemoveAll(fun x-> x.id=w.id) 
                        |> fun deleted_count->
                            match deleted_count with
                                |0->tools.error "couldnt deelete word"
                                |_ -> tools.debug " deleted word from new words list"
                        words.save_words_list words_reviewing words.reviewing_path
        
        let review_word() = 
             match word_now with
             |None-> tools.error "no word to review"
             | Some _word_now->
                tools.debug "reviewing"
                words_reviewing
                |> tools.mutable_list_to_list
                |> words.get_random_word 
                |> fun random_w->
                    match random_w with
                    |None-> tools.error "no random word"
                    |Some w-> 
                        word_to_text_component w
                        word_now<-Some w
        
        let on_deleted_but_can_continue (w:Word)=
                        words_deleted.Add(w)
                        words.save_words_list words_reviewing words.reviewing_path
                        words.save_words_list words_deleted words.deleted_words
                        review_word()

        let on_deleted_word (w:Word)(count:int)=
            tools.debug count
            match count with
                |0-> no_words_to_review()
                |_-> on_deleted_but_can_continue w

        let delete_word()= 
            match word_now with
            |None-> tools.error "no word defined"
            |Some word_n ->
                tools.debug <| System.String.Format("deleting word {0}",word_n.id)
                words_reviewing.Find(fun w->w.id = word_n.id) |> words_reviewing.Remove 
                    |> fun b -> 
                    match b with
                        |false ->tools.error "failed deleting word"
                        |true->
                            tools.debug "removed word"
                            words_reviewing.Count |> fun c->on_deleted_word word_n c



        tools.add_listener newword_button new_word
        tools.add_listener review_button review_word
        tools.add_listener learned_button delete_word
        tools.add_listener reset_button words.reset
