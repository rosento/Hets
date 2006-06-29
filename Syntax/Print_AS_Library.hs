{- |
Module      :  $Header$
Copyright   :  (c) Klaus L�ttich, Uni Bremen 2002-2006
License     :  similar to LGPL, see HetCATS/LICENSE.txt or LIZENZ.txt

Maintainer  :  luettich@tzi.de
Stability   :  provisional
Portability :  non-portable(Grothendieck)

pretty printing for heterogenous libraries in HetCASL.
-}

module Syntax.Print_AS_Library where

import Common.PrettyPrint
import Common.Id
import Common.Doc
import Common.DocUtils
import Common.Keywords

import Syntax.AS_Structured
import Syntax.AS_Library
import Common.AS_Annotation

import Syntax.Print_AS_Architecture()
import Syntax.Print_AS_Structured

instance PrettyPrint LIB_DEFN where
    printText0 = toOldText

instance Pretty LIB_DEFN where
    pretty (Lib_defn aa ab _ ad) =
        let aa' = pretty aa              -- lib name
            ab' = vsep $ map pretty ab -- LIB_ITEMs
            ad' = vcat $ map pretty ad -- global ANNOTATIONs
        in keyword libraryS <+> aa' $++$ ad' $++$ ab'

instance PrettyPrint LIB_ITEM where
    printText0 = toOldText

instance Pretty LIB_ITEM where
    pretty li = case li of
        Spec_defn si (Genericity aa ab _) ac _ ->
            let x : r = case skip_Group $ item ac of
                          Extension e@(_ : _) _ ->
                              printExtension $ moveAnnos ac e
                          Union u@(_ : _) _ ->
                              printUnion $ moveAnnos ac u
                          _ -> [pretty ac]
                sphead = fcat $ indexed (tokStr si) : printPARAMS aa
                        ++ printIMPORTED ab ++ [space <> equals]
             in vcat $ (topKey specS <+> vcat [sphead, x]) : r
                    ++ [keyword endS]
        View_defn si (Genericity aa ab _) (View_type frm to _) ad _ ->
            let sphead = fcat $ structSimpleId si : printPARAMS aa
                        ++ printIMPORTED ab ++ [space <> colon]
            in topKey viewS <+>
                 fsep ([sphead, sep [printGroupSpec frm <+> keyword toS,
                         (if null ad then id
                          else (<+> equals)) $ printGroupSpec to]]
                        ++ punctuate comma (map pretty ad))
                            $+$ keyword endS
        Arch_spec_defn si ab _ ->
            keyword archS <+> keyword specS <+>
                    fsep[structSimpleId si, equals, pretty ab]
                            $+$ keyword endS
        Unit_spec_defn si ab _ ->
            keyword unitS <+> keyword specS <+>
                    fsep[structSimpleId si, equals, pretty ab]
                            $+$ keyword endS
        Ref_spec_defn si ab _ ->
            keyword refinementS <+>
                    fsep[structSimpleId si, equals, pretty ab]
                            $+$ keyword endS
        Download_items l ab _ ->
            topKey fromS <+> fsep ([pretty l, keyword getS] ++
                                   punctuate comma (map pretty ab))
        Syntax.AS_Library.Logic_decl aa _ ->
            keyword logicS <+> pretty aa

instance PrettyPrint ITEM_NAME_OR_MAP where
    printText0 = toOldText

instance Pretty ITEM_NAME_OR_MAP where
    pretty l = case l of
        Item_name aa -> structSimpleId aa
        Item_name_map aa ab _ ->
            fsep [structSimpleId aa, mapsto, structSimpleId ab]

instance Pretty LIB_NAME where
    pretty l = case l of
        Lib_version i v ->
            pretty i <+> keyword versionS <+> pretty v
        Lib_id i -> pretty i

instance PrettyPrint LIB_NAME where
    printText0 = toOldText

instance Pretty LIB_ID where
    pretty l = structId $ case l of
        Direct_link u _ -> u
        Indirect_link p _ -> p

instance PrettyPrint LIB_ID where
    printText0 = toOldText

instance Pretty VERSION_NUMBER where
    pretty (Version_number aa _) =
        hcat $ punctuate dot $ map (pretty . mkSimpleId) aa

instance PrettyPrint VERSION_NUMBER where
    printText0 = toOldText
